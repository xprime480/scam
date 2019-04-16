#include "form/CallCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

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

void CallCont::run(ScamExpr * expr)
{
    if ( expr->error() ) {
        cont->run(expr);
        return;
    }

    if ( ! expr->hasApply() ) {
        ScamExpr * err =
            ExpressionFactory::makeError("call/cc: form ",
                                         expr->toString(),
                                         "cannot be applied");
        cont->run(err);
        return;
    }

    ScamExpr * contExpr = ExpressionFactory::makeContinuation(cont);
    ScamExpr * args = ExpressionFactory::makeList(contExpr);
    expr->apply(args, cont, env);
}