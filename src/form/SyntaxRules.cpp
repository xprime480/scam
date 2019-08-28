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

#include "util/GlobalId.hpp"
#include "util/DebugTrace.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;

SyntaxRules::SyntaxRules()
    : valid(false)
    , name(makeNothing())
    , defined(nullptr)
{
}

SyntaxRules::SyntaxRules(ScamEngine * engine,
                         ScamValue name,
                         ScamValue spec,
                         Env * env)
    : valid(false)
    , name(name)
    , defined(env)
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

void SyntaxRules::applySyntax(ScamValue args,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine)
{
    SyntaxMatchData data;
    SyntaxRule * match = findSyntaxRule(args, data);
    ScamValue expansion = expandWith(args, engine, data, match);

    if ( ! isNothing(expansion) ) {
        Env * applicationEnv = extendDefinition(env, match);

        LambdaDef expanded;
        expanded.valid = true;
        expanded.forms = expansion;
        ScamValue closure = makeClosure(expanded, applicationEnv);
        apply(closure, makeNull(), cont, applicationEnv, engine);
    }
}

ScamValue SyntaxRules::expandSyntax(ScamValue args, ScamEngine * engine)
{
    return expand(args, engine);
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
        if ( extractReserved(p1.value, engine) ) {
            rv = p2.value;
        }
    }

    return rv;
}

bool SyntaxRules::extractReserved(ScamValue syms, ScamEngine * engine)
{
    reserved.clear();

    while ( ! isNull(syms) ) {
        ScamValue sym = getCar(syms);
        syms          = getCdr(syms);

        if ( ! isSymbol(sym) ) {
            ScamValue err = badReserved(sym);
            engine->handleError(err);
            return false;
        }

        reserved.insert(sym->stringValue());
    }

    return true;
}

bool SyntaxRules::decodeRule(ScamValue rule, ScamEngine * engine)
{
    SyntaxRule * sr =
        standardMemoryManager.make<SyntaxRule>(rule, engine, name, reserved);

    bool rv = sr->isValid();
    if ( rv ) {
        rules.push_back(sr);
    }

    return rv;
}

ScamValue SyntaxRules::expand(ScamValue args, ScamEngine * engine)
{
    SyntaxMatchData data;
    SyntaxRule * match = findSyntaxRule(args, data);
    return expandWith(args, engine, data, match);
}

Env * SyntaxRules::extendDefinition(Env * base, SyntaxRule * match)
{
    GlobalId id;
    ScamTraceScope _;
    scamTrace(id, __FILE__, __LINE__, __FUNCTION__);

    Env * env = base->extend();

    set<ScamValue> syms = match->getFreeSymbols();
    for ( auto & s : syms ) {
        scamTrace(id, __FILE__, __LINE__, __FUNCTION__, writeValue(s));
        if ( truth(defined->check(s)) ) {
	    ScamValue v = defined->get(s);
	    scamTrace(id, __FILE__, __LINE__, __FUNCTION__, writeValue(v), v->isImmutable());
            env->put(s, v);
        }
        else {
            scamTrace(id, __FILE__, __LINE__, __FUNCTION__);
        }
    }

    env->dump(1, true, false);
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

ScamValue SyntaxRules::expandWith(ScamValue args,
                                  ScamEngine * engine,
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
        engine->handleError(rv);
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
