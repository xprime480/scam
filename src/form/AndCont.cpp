#include "form/AndCont.hpp"

#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/AndWorker.hpp"

using namespace scam;
using namespace std;

AndCont::AndCont(ScamValue args,
                 Continuation * cont,
                 Env * env,
                 ScamEngine * engine)
    : Continuation("And", engine)
    , args(args)
    , cont(cont)
    , env(env)
{
}

AndCont * AndCont::makeInstance(ScamValue args,
                                Continuation * cont,
                                Env * env,
                                ScamEngine * engine)
{
    return new AndCont(args, cont, env, engine);
}

void AndCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void AndCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        engine->handleError(value);
    }
    else if ( (! truth(value)) || isNull(args) ) {
        cont->handleValue(value);
    }
    else {
        workQueueHelper<AndWorker>(cont, env, args, engine);
    }
}
