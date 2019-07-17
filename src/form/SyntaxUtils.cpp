#include "form/SyntaxUtils.hpp"

#include "Env.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "util/Parameter.hpp"
#include "util/LambdaDef.hpp"

#include "util/GlobalId.hpp"
#include "util/DebugTrace.hpp"
#include "expr/ValueWriter.hpp"

bool scam::installSyntax(Env * env,
                         ScamEngine * engine,
                         ScamValue symbol,
                         ScamValue rules)
{
    GlobalId id;
    //ScamTraceScope _;
    scamTrace(id, __FILE__, __LINE__, __FUNCTION__,
              writeValue(symbol), writeValue(rules));
    const char * name = symbol->stringValue().c_str();

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
            lambda.rest    = makeNothing();
            lambda.forms   = sp1.value;

            ScamValue value = makeClosure(lambda, env, true);
            scamTrace(id, __FILE__, __LINE__, __FUNCTION__, writeValue(value));
            env->put(symbol, value);
            return true;
        }
    }

    return false;

}
