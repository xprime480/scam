#include "form/SyntaxRules.hpp"

#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
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
    , defined(nullptr)
{
}

SyntaxRules::SyntaxRules(ScamValue name, ScamValue spec, Env * env)
    : valid(false)
    , name(name)
    , defined(env)
{
    ScamValue rules = extractRules(spec);
    if ( isNothing(rules) ) {
        return;
    }

    while ( ! isNull(rules) ) {
        ScamValue head = getCar(rules);
        rules          = getCdr(rules);

        if ( ! decodeRule(head) ) {
            ScamValue err = invalidSyntax(head);
            ScamEngine::getEngine().handleError(err);
            return;
        }
    }

    valid = true;
}

void SyntaxRules::mark() const
{
    name->mark();
    defined->mark();

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

void SyntaxRules::applySyntax(ScamValue args, Continuation * cont, Env * env)
{
    SyntaxMatchData data;
    SyntaxRule * match = findSyntaxRule(args, data);
    ScamValue expansion = expandWith(args, data, match);

    if ( ! isNothing(expansion) ) {
        Env * applicationEnv = extendDefinition(env, match);

        LambdaDef expanded;
        expanded.valid = true;
        expanded.forms = expansion;
        ScamValue closure = makeClosure(expanded, applicationEnv);
        apply(closure, makeNull(), cont, applicationEnv);
    }
}

ScamValue SyntaxRules::expandSyntax(ScamValue args)
{
    return expand(args);
}

ScamValue SyntaxRules::extractRules(ScamValue spec)
{
    const char * chName = name->stringValue().c_str();

    SymbolParameter  p0;
    ListParameter    p1;
    ObjectParameter  pObj;
    ListOfParameter  pList(pObj);
    CountedParameter p2(pList);

    ScamValue rv = makeNothing();
    if ( argsToParms(spec, chName, p0, p1, p2) ) {
        if ( extractReserved(p1.value) ) {
            rv = p2.value;
        }
    }

    return rv;
}

bool SyntaxRules::extractReserved(ScamValue syms)
{
    reserved.clear();

    while ( ! isNull(syms) ) {
        ScamValue sym = getCar(syms);
        syms          = getCdr(syms);

        if ( ! isSymbol(sym) ) {
            ScamValue err = badReserved(sym);
            ScamEngine::getEngine().handleError(err);
            return false;
        }

        reserved.insert(sym->stringValue());
    }

    return true;
}

bool SyntaxRules::decodeRule(ScamValue rule)
{
    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    SyntaxRule * sr = mm.make<SyntaxRule>(rule, name, reserved);

    bool rv = sr->isValid();
    if ( rv ) {
        rules.push_back(sr);
    }

    return rv;
}

ScamValue SyntaxRules::expand(ScamValue args)
{
    SyntaxMatchData data;
    SyntaxRule * match = findSyntaxRule(args, data);
    return expandWith(args, data, match);
}

Env * SyntaxRules::extendDefinition(Env * base, SyntaxRule * match)
{
    Env * env = base->extend();

    set<ScamValue> syms = match->getFreeSymbols();
    ScamValue forwarder = makeForwarder(defined);
    for ( const auto & s : syms ) {
        if ( truth(defined->check(s)) ) {
            env->put(s, forwarder);
        }
    }

    return env;
}

SyntaxRule * SyntaxRules::findSyntaxRule(ScamValue args, SyntaxMatchData & data)
{
    for ( SyntaxRule * rule : rules ) {
        data.clear();
        if ( rule->match(args, data) ) {
            return rule;
        }
    }

    return nullptr;
}

ScamValue
SyntaxRules::expandWith(ScamValue args,
                        SyntaxMatchData & data,
                        SyntaxRule * match)
{
    ScamValue rv = makeNothing();
    if ( match && match->isValid() ) {
        rv = match->expand(data);
    }
    else {
        rv = invalidExpansion(args);
    }

    if ( isUnhandledError(rv) ) {
        ScamEngine::getEngine().handleError(rv);
        rv = makeNothing();
    }

    return rv;
}

ScamValue SyntaxRules::badReserved(ScamValue value)
{
    static const char * msg { "only symbols allowed in reserved words (%{0})" };
    ScamValue err = makeError(msg, value);
    err->errorCategory() = syntaxCategory;
    return err;
}

ScamValue SyntaxRules::invalidSyntax(ScamValue rule)
{
    static const char * msg { "invalid pattern: %{0} for syntax %{1}" };
    ScamValue err = makeError(msg, rule, name);
    err->errorCategory() = syntaxCategory;
    return err;
}

ScamValue SyntaxRules::invalidExpansion(ScamValue args)
{
    static const char * msg
    { "no pattern matches for syntax %{0} given args %{1}" };
    ScamValue err = makeError(msg, name, args);
    err->errorCategory() = syntaxCategory;
    return err;
}
