#include "form/SyntaxRule.hpp"

#include "ScamEngine.hpp"
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
    bool isLiteral(ScamValue pat)
    {
        return ( isString(pat) ||
                 isChar(pat) ||
                 isBoolean(pat) ||
                 isNumeric(pat) ||
                 isKeyword(pat) );
    }
}

SyntaxRule::SyntaxRule(ScamValue rule, ScamEngine * engine, ScamValue name)
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
        pattern = parsePattern(pat, engine);

        templat = parseTemplate(sp1.value);

        if ( pattern && templat ) {
            valid = true;
        }
    }
}

void SyntaxRule::mark() const
{
    if ( pattern ) {
        pattern->mark();
    }
    if ( templat ) {
        templat->mark();
    }
    name->mark();
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

    return pattern->match(args, data);
}

ScamValue SyntaxRule::expand(const SyntaxMatchData & data)
{
    if ( templat ) {
        return templat->expand(data);

    }
    else {
        return makeNothing();
    }
}

PatternData * SyntaxRule::parsePattern(ScamValue pat, ScamEngine * engine)
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

            PatternData * temp = parsePattern(head, engine);
            if ( nullptr == temp ) {
                return nullptr;
            }

            vec.push_back(temp);
        }

        if ( isSymbol(pat) ) {
            PatternData * temp = mm.make<PatternDataIdentifier>(pat, true);
            vec.push_back(temp);
        }
        else if ( ! isNull(pat) ) {
            ScamValue err = invalidPattern(orig);
            engine->handleError(err);
            return nullptr;
        }

        rv = mm.make<PatternDataSequence>(vec);
    }

    else if ( isSymbol(pat) ) {
        rv = mm.make<PatternDataIdentifier>(pat);
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
        vector<TemplateData *> subs;

        while ( ! isNull(tem) ) {
            ScamValue head = getCar(tem);
            tem            = getCdr(tem);
            TemplateData * t = parseTemplate(head);
            subs.push_back(t);
        }

        rv = mm.make<TemplateDataList>(subs);
    }

    else if ( isSymbol(tem) ) {
        rv = mm.make<TemplateDataIdentifier>(tem);
    }

    else {
        rv = mm.make<TemplateDataLiteral>(tem);
    }

    return rv;
}

ScamValue SyntaxRule::invalidPattern(ScamValue pat)
{
    static const char * msg { "invalid pattern: %{0} in syntax %{1}" };
    return makeError(msg, pat, name);
}
