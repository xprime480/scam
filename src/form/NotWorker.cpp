#include "form/NotWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "form/NotCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"

using namespace scam;
using namespace std;

NotWorker::NotWorker(Continuation * cont, Env * env, ScamValue value)
    : Worker("Not")
    , value(value)
    , cont(cont)
    , env(env)
{
}

NotWorker *
NotWorker::makeInstance(Continuation * cont, Env * env, ScamValue value)
{
    return new NotWorker(cont, env, value);
}

void NotWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        value->mark();
        cont->mark();
        env->mark();
    }
}

void NotWorker::run()
{
    Worker::run();

    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    Continuation * newCont = mm.make<NotCont>(cont);
    eval(value, newCont, env);
}
