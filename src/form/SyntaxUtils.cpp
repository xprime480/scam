#include "form/SyntaxUtils.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "util/LambdaDef.hpp"
#include "util/Parameter.hpp"

#include <map>

using namespace scam;
using namespace std;

namespace
{
    using FormalsToActuals = map<string, ScamValue>;

    ScamValue substitute(const LambdaDef & lambda, ScamValue actuals);
    ScamValue substituteForm(ScamValue form, const FormalsToActuals & f2a);
    ScamValue conformalError(ScamValue formals, ScamValue actuals);
}

bool scam::installSyntax(Env * env,
                         ScamEngine * engine,
                         ScamValue symbol,
                         ScamValue rules)
{
    const char * name = symbol->stringValue().c_str();

    SymbolParameter p0;
    ListParameter   p1;
    ObjectParameter pObj;
    ListOfParameter p2(pObj);

    if ( argsToParms(rules, engine, name, p0, p1, p2) ) {
        ScamValue def = p2.value;
        ListParameter    sp0;
        ObjectParameter  spObj;
        CountedParameter sp1(spObj);
        if ( argsToParms(def, engine, name, sp0, sp1) ) {
            LambdaDef lambda;
            lambda.formals = getCdr(sp0.value);
            lambda.forms   = sp1.value;

            ScamValue value = makeSyntax(name, lambda);
            env->put(symbol, value);
            return true;
        }
    }

    return false;
}

void scam::applySyntax(ScamValue value,
                       ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    const LambdaDef & def = value->syntaxDef();
    LambdaDef expanded;
    expanded.valid = true;
    expanded.forms = substitute(def, args);

    ScamValue closure = makeClosure(expanded, env, false);
    apply(closure, makeNull(), cont, env, engine);
}

namespace
{
    ScamValue substitute(const LambdaDef & lambda, ScamValue actuals)
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

    ScamValue substituteForm(ScamValue form, const FormalsToActuals & f2a)
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

    ScamValue conformalError(ScamValue formals, ScamValue actuals)
    {
        static const char * msg
        { "formals are not conformal with actuals (%{0}) (%{1})" };
        return makeError(msg, formals, actuals);
    }
}
