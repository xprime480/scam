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

void CallCont::handleValue(ScamValue expr)
{
    Continuation::handleValue(expr);

    if ( isError(expr) ) {
        engine->handleError(expr);
        return;
    }

    if ( ! isApplicable(expr) ) {
        ScamValue err = makeErrorExtended("call/cc: form ",
                                          writeValue(expr),
                                          "cannot be applied");
        engine->handleError(err);
        return;
    }

    ScamValue contExpr = makeContinuation(cont);
    ScamValue args = makeList(contExpr);
    apply(expr, args, cont, env, engine);
}
