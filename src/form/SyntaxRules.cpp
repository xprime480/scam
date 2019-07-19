#include "form/SyntaxRules.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
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
    for ( const auto & d : defs ) {
        d.mark();
    }
}

ScamValue SyntaxRules::getName() const
{
    return name;
}

void SyntaxRules::applySyntax(ScamValue args,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine)
{
    LambdaDef match = findSyntax(args);

    if ( match.valid ) {
        LambdaDef expanded;
        expanded.valid = true;
        expanded.forms = substitute(match, args);

        ScamValue closure = makeClosure(expanded, env, false);
        apply(closure, makeNull(), cont, env, engine);
    }
    else {
        ScamValue err = invalidExpansion(args);
        engine->handleError(err);
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

    if ( argsToParms(spec, engine, chName, p0, p1, p2) ) {
        return p2.value;
    }
    else {
        return makeNothing();
    }
}

bool SyntaxRules::decodeRule(ScamValue rule, ScamEngine * engine)
{
    bool rv = false;

    const char * chName = name->stringValue().c_str();

    ListParameter    sp0;
    ObjectParameter  spObj;
    CountedParameter sp1(spObj);

    if ( argsToParms(rule, engine, chName, sp0, sp1) ) {
        LambdaDef lambda;
        lambda.valid   = true;
        lambda.formals = getCdr(sp0.value);
        lambda.forms   = sp1.value;
        defs.push_back(lambda);
        rv = true;
    }

    return rv;
}

LambdaDef SyntaxRules::findSyntax(ScamValue args)
{
    const auto argCount = length(args);

    for ( const LambdaDef & def : defs ) {
        if ( argCount == length(def.formals) ) {
            return def;
        }
    }

    LambdaDef noMatch;
    return noMatch;
}

ScamValue SyntaxRules::substitute(const LambdaDef & lambda, ScamValue actuals)
{
    FormalsToActuals f2a;
    ScamValue f = lambda.formals;
    ScamValue a = actuals;

    while ( ! isNull(f) ) {
        if ( isNull(a) ) {
            return conformalError(lambda.formals, actuals);
        }

        const string & key = getCar(f)->stringValue();
        ScamValue      val = getCar(a);
        f2a[key] = val;

        f = getCdr(f);
        a = getCdr(a);
    }

    if ( ! isNull(a) ) {
        return conformalError(lambda.formals, actuals);
    }

    ScamValue rv = substituteForm(lambda.forms, f2a);
    return rv;
}

ScamValue
SyntaxRules::substituteForm(ScamValue form, const FormalsToActuals & f2a)
{
    if ( f2a.empty() ) {
        return form;
    }

    ScamValue rv = form;

    if ( isList(form) ) {
        vector<ScamValue> newForms;
        while ( ! isNull(form) ) {
            ScamValue head = getCar(form);
            form           = getCdr(form);
            ScamValue newValue = substituteForm(head, f2a);
            newForms.push_back(newValue);
        }
        rv = makeList(newForms);
    }

    else if ( isSymbol(form) ) {
        const string & name = form->stringValue();
        const auto iter = f2a.find(name);
        if ( f2a.end() != iter ) {
            rv = iter->second;
        }
    }

    return rv;
}

ScamValue SyntaxRules::invalidSyntax(ScamValue rule)
{
    static const char * msg { "invalid pattern: %{0} for syntax %{1}" };
    return makeError(msg, rule, name);
}

ScamValue SyntaxRules::invalidExpansion(ScamValue args)
{
    static const char * msg
    { "no pattern matches for syntax %{0} given args %{1}" };
    return makeError(msg, name, args);
}

ScamValue SyntaxRules::conformalError(ScamValue formals, ScamValue actuals)
{
    static const char * msg
    { "formals are not conformal with actuals (%{0}) (%{1})" };
    return makeError(msg, formals, actuals);
}
