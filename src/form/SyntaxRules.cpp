#include "form/SyntaxRules.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "form/SyntaxMatchData.hpp"
#include "util/LambdaDef.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

SyntaxRules::SyntaxRules()
    : valid(false)
    , name(makeNothing())
{
}

SyntaxRules::SyntaxRules(ScamEngine * engine, ScamValue name, ScamValue spec)
    : valid(false)
    , name(name)
{
    ScamValue rules = extractRules(spec, engine);
    if ( isNothing(rules) ) {
        return;
    }

    while ( ! isNull(rules) ) {
        ScamValue head = getCar(rules);
        rules          = getCdr(rules);

        if ( ! decodeRule(head, engine) ) {
            ScamValue err = invalidSyntax(head);
            engine->handleError(err);
            return;
        }
    }

    valid = true;
}

void SyntaxRules::mark() const
{
    name->mark();
    for ( const auto & r : rules ) {
        r->mark();
    }
}

ScamValue SyntaxRules::getName() const
{
    return name;
}

bool SyntaxRules::isValid() const
{
    return valid;
}

void SyntaxRules::applySyntax(ScamValue args,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine)
{
    ScamValue expansion = expand(args, env, engine);

    if ( ! isNothing(expansion) ) {
        LambdaDef expanded;
        expanded.valid = true;
        expanded.forms = expansion;
        ScamValue closure = makeClosure(expanded, env, false);
        apply(closure, makeNull(), cont, env, engine);
    }
}

ScamValue SyntaxRules::extractRules(ScamValue spec, ScamEngine * engine)
{
    const char * chName = name->stringValue().c_str();

    SymbolParameter  p0;
    ListParameter    p1;
    ObjectParameter  pObj;
    ListOfParameter  pList(pObj);
    CountedParameter p2(pList);

    ScamValue rv = makeNothing();
    if ( argsToParms(spec, engine, chName, p0, p1, p2) ) {
        rv = p2.value;
    }

    return rv;
}

bool SyntaxRules::decodeRule(ScamValue rule, ScamEngine * engine)
{
    SyntaxRule * sr = standardMemoryManager.make<SyntaxRule>(rule, engine, name);
    bool rv = sr->isValid();
    if ( rv ) {
        rules.push_back(sr);
    }

    return rv;
}

ScamValue SyntaxRules::expand(ScamValue args, Env * env, ScamEngine * engine)
{
    SyntaxMatchData data;
    SyntaxRule * match = findSyntaxRule(args, data);

    ScamValue rv = makeNothing();

    if ( match ) {
        rv = match->expand(data);
    }
    else {
        ScamValue err = invalidExpansion(args);
        engine->handleError(err);
    }

    return rv;
}

SyntaxRule * SyntaxRules::findSyntaxRule(ScamValue args, SyntaxMatchData & data)
{
    for ( SyntaxRule * rule : rules ) {
        if ( rule->match(args, data) ) {
            return rule;
        }
    }

    return nullptr;
}

ScamValue SyntaxRules::invalidSyntax(ScamValue rule)
{
    static const char * msg { "invalid pattern: %{0} for syntax %{1}" };
    return makeError(msg, rule, name);
    return nullptr;
}

ScamValue SyntaxRules::invalidExpansion(ScamValue args)
{
    static const char * msg
    { "no pattern matches for syntax %{0} given args %{1}" };
    return makeError(msg, name, args);
}
