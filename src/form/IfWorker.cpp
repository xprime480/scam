#include "form/IfWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "form/IFCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

IfWorker::IfWorker(Continuation * cont,
                   Env * env,
                   ScamEngine * engine,
                   ScamValue args)
    : Worker("If", engine)
    , args(args)
    , cont(cont)
    , env(env)
{
}

IfWorker * IfWorker::makeInstance(Continuation * cont,
                                  Env * env,
                                  ScamEngine * engine,
                                  ScamValue args)
{
    return new IfWorker(cont, env, engine, args);
}

void IfWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        cont->mark();
        env->mark();
        args->mark();
    }
}

void IfWorker::run()
{
    Worker::run();

    Continuation * newCont =
        standardMemoryManager.make<IfCont>(args, cont, env, engine);
    ScamValue test = nthcar(args, 0);
    eval(test, newCont, env, engine);
}
