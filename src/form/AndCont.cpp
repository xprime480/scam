#include "form/AndCont.hpp"

#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "form/AndWorker.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"

using namespace scam;
using namespace std;

AndCont::AndCont(ScamValue args, Continuation * cont, Env * env)
    : Continuation("And")
    , args(args)
    , cont(cont)
    , env(env)
{
}

AndCont * AndCont::makeInstance(ScamValue args, Continuation * cont, Env * env)
{
    return new AndCont(args, cont, env);
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
        ScamEngine::getEngine().handleError(value);
    }
    else if ( (! truth(value)) || isNull(args) ) {
        cont->handleValue(value);
    }
    else {
        workQueueHelper<AndWorker>(cont, env, args);
    }
}
