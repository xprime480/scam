#include "form/ApplyArgsCont.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

ApplyArgsCont::ApplyArgsCont(ExprHandle op, Continuation * cont, Env * env)
    : Continuation("apply args")
    , op(op)
    , cont(cont)
    , env(env)
{
}

ApplyArgsCont *
ApplyArgsCont::makeInstance(ExprHandle op, Continuation * cont, Env * env)
{
    return new ApplyArgsCont(op, cont, env);
}

void ApplyArgsCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        op->mark();
        cont->mark();
        env->mark();
    }
}

void ApplyArgsCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else {
        op->apply(expr, cont, env);
    }
}
