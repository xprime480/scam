#include "form/ApplyOpCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/ApplyArgsWorker.hpp"

using namespace scam;
using namespace std;

ApplyOpCont::ApplyOpCont(ScamValue args, Continuation * cont, Env * env)
    : Continuation("apply")
    , args(args)
    , cont(cont)
    , env(env)
{
}

ApplyOpCont *
ApplyOpCont::makeInstance(ScamValue args, Continuation * cont, Env * env)
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

void ApplyOpCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        cont->run(expr);
    }
    else {
        workQueueHelper<ApplyArgsWorker>(expr, args, cont, env);
    }
}
