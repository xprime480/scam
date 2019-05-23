#include "form/CallCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"

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
    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
        return;
    }

    if ( ! expr->hasApply() ) {
        ScamValue err =
            ExpressionFactory::makeError("call/cc: form ",
                                         ExprWriter::write(expr),
                                         "cannot be applied");
        cont->run(err);
        return;
    }

    ScamValue contExpr = ExpressionFactory::makeContinuation(cont);
    ScamValue args = ExpressionFactory::makeList(contExpr);
    expr->apply(args, cont, env);
}
