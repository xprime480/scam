#include "form/OrCont.hpp"

#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/OrWorker.hpp"

using namespace scam;
using namespace std;

OrCont::OrCont(ScamValue args, Continuation * cont, Env * env)
    : Continuation("Or")
    , args(args)
    , cont(cont)
    , env(env)
{
}

OrCont * OrCont::makeInstance(ScamValue args, Continuation * cont, Env * env)
{
    return new OrCont(args, cont, env);
}

void OrCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void OrCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else if ( truth(value) || isNull(args) ) {
        cont->handleValue(value);
    }
    else {
        workQueueHelper<OrWorker>(cont, env, args);
    }
}
