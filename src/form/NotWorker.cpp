#include "form/NotWorker.hpp"

#include "Continuation.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "form/NotCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

NotWorker::NotWorker(Continuation * cont,
                     Env * env,
                     ScamEngine * engine,
                     ScamValue value)
    : Worker("Not", engine)
    , value(value)
    , cont(cont)
    , env(env)
{
}

NotWorker * NotWorker::makeInstance(Continuation * cont,
                                    Env * env,
                                    ScamEngine * engine,
                                    ScamValue value)
{
    return new NotWorker(cont, env, engine, value);
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

    Continuation * newCont = standardMemoryManager.make<NotCont>(cont, engine);
    eval(value, newCont, env, engine);
}
