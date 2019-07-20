#include "form/SyntaxRule.hpp"

#include "ScamEngine.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/PatternData.hpp"
#include "form/SyntaxMatchData.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

SyntaxRule::SyntaxRule(ScamValue rule, ScamEngine * engine, ScamValue name)
    : valid(false)
    , pattern(nullptr)
    , templat(makeNothing())
    , name(name)
{
    const char * chName = name->stringValue().c_str();

    ObjectParameter  sp0;
    ObjectParameter  spObj;
    CountedParameter sp1(spObj);


    if ( argsToParms(rule, engine, chName, sp0, sp1) ) {
        templat = sp1.value;
        ScamValue pat = makePair(makeNothing(), getCdr(sp0.value));
        pattern = parsePattern(pat, engine);
        if ( pattern ) {
            valid = true;
        }
    }
}

void SyntaxRule::mark() const
{
    if ( pattern ) {
        pattern->mark();
    }
    templat->mark();
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

ScamValue SyntaxRule::substitute(const SyntaxMatchData & data)
{
    ScamValue rv = substituteForm(templat, data);
    return rv;
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

    else {
        ScamValue err = invalidPattern(pat);
        engine->handleError(err);
    }

    return rv;
}

ScamValue
SyntaxRule::substituteForm(ScamValue form, const SyntaxMatchData & data)
{
    if ( data.data.empty() ) {
        return form;
    }

    ScamValue rv = form;

    if ( isList(form) ) {
        vector<ScamValue> newForms;

        while ( ! isNull(form) ) {
            ScamValue head = getCar(form);
            form           = getCdr(form);
            ScamValue newValue = substituteForm(head, data);
            newForms.push_back(newValue);
        }

        rv = makeList(newForms);
    }

    else if ( isSymbol(form) ) {
        const string & name = form->stringValue();
        const auto iter = data.data.find(name);
        if ( data.data.end() != iter ) {
            rv = iter->second;
        }
    }

    return rv;
}

ScamValue SyntaxRule::invalidPattern(ScamValue pat)
{
    static const char * msg { "invalid pattern: %{0} in syntax %{1}" };
    return makeError(msg, pat, name);
}
