#include "form/CallCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;

CallCont::CallCont(Continuation * cont, Env * env, ScamEngine * engine)
    : Continuation("CallCont", engine)
    , cont(cont)
    , env(env)
{
}

CallCont *
CallCont::makeInstance(Continuation * cont, Env * env, ScamEngine * engine)
{
    return new CallCont(cont, env, engine);
}

void CallCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
        env->mark();
    }
}

void CallCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isError(value) ) {
        engine->handleError(value);
        return;
    }

    if ( ! isApplicable(value) ) {
        ScamValue err = makeErrorExtended("call/cc: form ",
                                          writeValue(value),
                                          "cannot be applied");
        engine->handleError(err);
        return;
    }

    ScamValue contExpr = makeContinuation(cont);
    ScamValue args = makeList(contExpr);
    apply(value, args, cont, env, engine);
}
