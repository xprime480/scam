#include "form/ApplyOpCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "form/ApplyArgsWorker.hpp"

using namespace scam;
using namespace std;

ApplyOpCont::ApplyOpCont(ScamExpr * args, Continuation * cont, Env * env)
    : Continuation("apply")
    , args(args)
    , cont(cont)
    , env(env)
{
}

ApplyOpCont *
ApplyOpCont::makeInstance(ScamExpr * args, Continuation * cont, Env * env)
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

void ApplyOpCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else {
        workQueueHelper<ApplyArgsWorker>(expr, args, cont, env);
    }
}
