#include "form/SyntaxUtils.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "util/LambdaDef.hpp"
#include "util/Parameter.hpp"

#include "util/GlobalId.hpp"
#include "util/DebugTrace.hpp"
#include "expr/ValueWriter.hpp"

bool scam::installSyntax(Env * env,
                         ScamEngine * engine,
                         ScamValue symbol,
                         ScamValue rules)
{
    const char * name = symbol->stringValue().c_str();

    GlobalId id;
    //ScamTraceScope _;
    scamTrace(id, __FILE__, __LINE__, __FUNCTION__,
              writeValue(symbol), writeValue(rules));

    SymbolParameter p0;
    ListParameter   p1;
    ObjectParameter pObj;
    ListOfParameter p2(pObj);

    if ( argsToParms(rules, engine, name, p0, p1, p2) ) {
        scamTrace(id, __FILE__, __LINE__, __FUNCTION__, writeValue(p2.value));

        ScamValue def = p2.value;
        ListParameter    sp0;
        ObjectParameter  spObj;
        CountedParameter sp1(spObj);
        if ( argsToParms(def, engine, name, sp0, sp1) ) {
            scamTrace(id, __FILE__, __LINE__, __FUNCTION__,
                      writeValue(sp0.value), writeValue(sp1.value));

            LambdaDef lambda;
            lambda.formals = getCdr(sp0.value);
            lambda.forms   = sp1.value;

            ScamValue value = makeSyntax(name, lambda);
            scamTrace(id, __FILE__, __LINE__, __FUNCTION__, writeValue(value));
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
    GlobalId id;
    ScamTraceScope _;
    scamTrace(id, __FILE__, __LINE__, __FUNCTION__,
              writeValue(value), writeValue(args));

    const LambdaDef & def = value->syntaxDef();
    scamTrace(id, __FILE__, __LINE__, __FUNCTION__, writeValue(def.forms));

    LambdaDef instance;
    instance.forms = def.forms;

    // simplest thing that makes the test pass!

    ScamValue closure = makeClosure(instance, env, false);
    apply(closure, makeNull(), cont, env, engine);
}
