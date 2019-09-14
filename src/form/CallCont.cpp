#include "form/CallCont.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

using namespace scam;
using namespace std;

CallCont::CallCont(Continuation * cont, Env * env)
    : Continuation("CallCont")
    , cont(cont)
    , env(env)
{
}

CallCont *
CallCont::makeInstance(Continuation * cont, Env * env)
{
    return new CallCont(cont, env);
}

void CallCont::mark()
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

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
        return;
    }

    if ( ! isApplicable(value) ) {
        ScamValue err =
            makeError("call/cc: form '%{0}' cannot be applied", value);
        err->errorCategory() = evalCategory;
        ScamEngine::getEngine().handleError(err);
        return;
    }

    ScamValue contExpr = makeContinuation(cont);
    ScamValue args = makeList(contExpr);
    apply(value, args, cont, env);
}
