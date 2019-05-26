#include "form/EvalCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

EvalCont::EvalCont(Continuation * cont, Env * env)
    : Continuation("eval")
    , cont(cont)
    , env(env)
{
}

EvalCont * EvalCont::makeInstance(Continuation * cont, Env * env)
{
    return new EvalCont(cont, env);
}

void EvalCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
        env->mark();
    }
}

void EvalCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        cont->run(expr);
    }
    else {
        eval(expr, cont, env->getTop());
    }
}
