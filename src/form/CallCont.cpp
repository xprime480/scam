#include "form/CallCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;

CallCont::CallCont(Continuation * cont, Env * env)
    : Continuation("CallCont")
    , cont(cont)
    , env(env)
{
}

CallCont * CallCont::makeInstance(Continuation * cont, Env * env)
{
    return new CallCont(cont, env);
}

void CallCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
        env->mark();
    }
}

void CallCont::run(ScamValue expr)
{
    if ( error(expr) ) {
        cont->run(expr);
        return;
    }

    if ( ! isApplicable(expr) ) {
        ScamValue err =
            ExpressionFactory::makeError("call/cc: form ",
                                         writeValue(expr),
                                         "cannot be applied");
        cont->run(err);
        return;
    }

    ScamValue contExpr = ExpressionFactory::makeContinuation(cont);
    ScamValue args = ExpressionFactory::makeList(contExpr);
    apply(expr, args, cont, env);
}
