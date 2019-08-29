#include "form/SyntaxRule.hpp"

#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/PatternData.hpp"
#include "form/SyntaxMatchData.hpp"
#include "form/TemplateData.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

namespace
{
    static const ScamValue dots = makeSymbol("...", false);

    bool isLiteral(ScamValue pat)
    {
        return ( isString(pat) ||
                 isChar(pat) ||
                 isBoolean(pat) ||
                 isNumeric(pat) ||
                 isKeyword(pat) );
    }
}

SyntaxRule::SyntaxRule(ScamValue rule,
                       ScamEngine * engine,
                       ScamValue name,
                       const set<string> & reserved)
    : valid(false)
    , pattern(nullptr)
    , templat(nullptr)
    , name(name)
{
    const char * chName = name->stringValue().c_str();

    ObjectParameter  sp0;
    ObjectParameter  spObj;
    CountedParameter sp1(spObj);

    if ( argsToParms(rule, engine, chName, sp0, sp1) ) {
        ScamValue pat = makePair(makeNothing(), getCdr(sp0.value));
        pattern = parsePattern(pat, engine, reserved);

        ScamValue tem = sp1.value;
        templat = parseTemplate(tem);

        if ( pattern && templat ) {
            valid = true;
	    freeSymbols = templat->getFreeSymbols();
        }
    }
}

SyntaxRule::~SyntaxRule()
{
}

SyntaxRule * SyntaxRule::makeInstance(ScamValue rule,
                                      ScamEngine * engine,
                                      ScamValue name,
                                      const set<string> & reserved)
{
    return new SyntaxRule(rule, engine, name, reserved);
}

void SyntaxRule::mark()
{
    if ( ! isMarked() ) {
        ManagedObject::mark();

        if ( pattern ) {
            pattern->mark();
        }
        if ( templat ) {
            templat->mark();
        }
	for ( auto s : freeSymbols ) {
	    s->mark();
	}
        name->mark();
    }
}

bool SyntaxRule::isValid() const
{
    return valid;
}

bool SyntaxRule::match(ScamValue args, SyntaxMatchData & data)
{
    if ( (! isValid()) || (nullptr == pattern) ) {
        return false;
    }

    bool rv = pattern->match(args, data);
    return rv;
}

ScamValue SyntaxRule::expand(const SyntaxMatchData & data)
{
    ScamValue rv = makeNothing();
    if ( templat ) {
        rv = templat->expand(data);
    }

    return rv;
}

set<ScamValue> SyntaxRule::getFreeSymbols() const
{
    return freeSymbols;
}

string SyntaxRule::identify() const
{
    stringstream s;
    s << "(" << pattern->identify();
    s << " " << templat->identify() << ")";
    return s.str();
}

PatternData * SyntaxRule::parsePattern(ScamValue pat,
                                       ScamEngine * engine,
                                       const set<string> & reserved)
{
    MemoryManager & mm = standardMemoryManager;
    PatternData * rv = nullptr;

    if ( isNothing(pat) ) {
        rv = mm.make<PatternDataNothing>();
    }

    else if ( isNull(pat) ) {
        vector<PatternData *> vec;
        rv =  mm.make<PatternDataSequence>(vec);
    }

    else if ( isPair(pat) ) {
        ScamValue orig = pat;

        vector<PatternData *> vec;
        while ( isPair(pat) ) {
            ScamValue head = getCar(pat);
            pat            = getCdr(pat);

            if ( equals(dots, head) ) {
                if ( vec.empty() ) {
                    invalidPattern(orig);
                    return nullptr;
                }

                vec.back()->tagAsEllipsis();
                continue;
            }

            PatternData * temp = parsePattern(head, engine, reserved);
            if ( nullptr == temp ) {
                return nullptr;
            }

            vec.push_back(temp);
        }

        if ( isSymbol(pat) ) {
            PatternData * temp = mm.make<PatternDataIdentifier>(pat, true);
            vec.push_back(temp);
            patternIdentifiers.insert(pat->stringValue());
        }
        else if ( ! isNull(pat) ) {
            ScamValue err = invalidPattern(orig);
            engine->handleError(err);
            return nullptr;
        }

        rv = mm.make<PatternDataSequence>(vec);
    }

    else if ( isSymbol(pat) ) {
        const auto iter = reserved.find(pat->stringValue());
        if ( reserved.end() == iter ) {
            rv = mm.make<PatternDataIdentifier>(pat);
            patternIdentifiers.insert(pat->stringValue());
        }
        else {
            rv = mm.make<PatternDataLiteral>(pat);
        }
    }

    else if ( isLiteral(pat) ) {
        rv = mm.make<PatternDataLiteral>(pat);
    }

    else {
        ScamValue err = invalidPattern(pat);
        engine->handleError(err);
    }

    return rv;
}

TemplateData * SyntaxRule::parseTemplate(ScamValue tem)
{
    MemoryManager & mm = standardMemoryManager;
    TemplateData * rv = nullptr;

    if ( isList(tem) ) {
        ScamValue orig = tem;
        vector<TemplateData *> subs;

        while ( ! isNull(tem) ) {
            ScamValue head = getCar(tem);
            tem            = getCdr(tem);

            if ( equals(dots, head) ) {
                if ( subs.empty() ) {
                    invalidTemplate(orig);
                    return nullptr;
                }

                TemplateData * t = subs.back();
                if ( dynamic_cast<TemplateDataLiteral *>(t) ) {
                    invalidTemplate(orig);
                    return nullptr;
                }

                subs.pop_back();
                t = mm.make<TemplateDataEllipsis>(t);
                subs.push_back(t);
                continue;
            }

            TemplateData * t = parseTemplate(head);
            if ( ! t ) {
                return t;
            }
            subs.push_back(t);
        }

        rv = mm.make<TemplateDataList>(subs);
    }

    else if ( isSymbol(tem) ) {
        if ( patternIdentifiers.end() ==
             patternIdentifiers.find(tem->stringValue()) ) {
            rv = mm.make<TemplateDataLiteral>(tem);
        }
        else {
            rv = mm.make<TemplateDataIdentifier>(tem);
        }
    }

    else {
        rv = mm.make<TemplateDataLiteral>(tem);
    }

    return rv;
}

ScamValue SyntaxRule::invalidPattern(ScamValue pat)
{
    static const char * msg { "invalid pattern: %{0} in syntax %{1}" };
    ScamValue err = makeError(msg, pat, name);
    err->errorCategory() = envCategory;
    return err;
}

ScamValue SyntaxRule::invalidTemplate(ScamValue tem)
{
    static const char * msg { "invalid template: %{0} in syntax %{1}" };
    ScamValue err = makeError(msg, tem, name);
    err->errorCategory() = envCategory;
    return err;
}
