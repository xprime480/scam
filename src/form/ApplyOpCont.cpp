#include "form/ApplyOpCont.hpp"

#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/ApplyArgsWorker.hpp"

using namespace scam;
using namespace std;

ApplyOpCont::ApplyOpCont(ScamValue args, Continuation * cont, Env * env)
    : Continuation("Apply")
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

void ApplyOpCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void ApplyOpCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        workQueueHelper<ApplyArgsWorker>(value, args, cont, env);
    }
}
