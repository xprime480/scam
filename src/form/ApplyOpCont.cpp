#include "form/ApplyOpCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/ApplyArgsWorker.hpp"

using namespace scam;
using namespace std;

ApplyOpCont::ApplyOpCont(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine)
    : Continuation("apply", engine)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ApplyOpCont * ApplyOpCont::makeInstance(ScamValue args,
                                        Continuation * cont,
                                        Env * env,
                                        ScamEngine * engine)
{
    return new ApplyOpCont(args, cont, env, engine);
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

    if ( isError(value) ) {
        engine->handleError(value);
    }
    else {
        workQueueHelper<ApplyArgsWorker>(value, args, cont, env, engine);
    }
}
