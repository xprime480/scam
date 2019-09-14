#include "form/ApplyArgsWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "form/ApplyArgsCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"

using namespace scam;
using namespace std;

ApplyArgsWorker::ApplyArgsWorker(ScamValue op,
                                 ScamValue args,
                                 Continuation * cont,
                                 Env * env)
    : Worker("Apply Args")
    , op(op)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ApplyArgsWorker * ApplyArgsWorker::makeInstance(ScamValue op,
                                                ScamValue args,
                                                Continuation * cont,
                                                Env * env)
{
    return new ApplyArgsWorker(op, args, cont, env);
}

void ApplyArgsWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        op->mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void ApplyArgsWorker::run()
{
    Worker::run();

    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    Continuation * newCont = mm.make<ApplyArgsCont>(op, cont, env);
    eval(args, newCont, env);
}
