#include "form/ApplyOpCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "form/ApplyArgsWorker.hpp"

using namespace scam;
using namespace std;

ApplyOpCont::ApplyOpCont(ExprHandle args, Continuation * cont, Env * env)
    : Continuation("apply")
    , args(args)
    , cont(cont)
    , env(env)
{
}

ApplyOpCont *
ApplyOpCont::makeInstance(ExprHandle args, Continuation * cont, Env * env)
{
    return new ApplyOpCont(args, cont, env);
}

void ApplyOpCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void ApplyOpCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
    }
    else {
        workQueueHelper<ApplyArgsWorker>(expr, args, cont, env);
    }
}
