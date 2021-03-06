#include "prim/PrimWorker.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "prim/PrimEvalCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"

using namespace scam;
using namespace std;

PrimWorker::PrimWorker(ScamValue caller,
                       ScamValue args,
                       Continuation * original,
                       Env * env)
    : Worker("Primitive")
    , args(args)
    , env(env)
{
    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    cont = mm.make<PrimEvalCont>(caller, original);
}

PrimWorker * PrimWorker::makeInstance(ScamValue caller,
                                      ScamValue args,
                                      Continuation * original,
                                      Env * env)
{
    return new PrimWorker(caller, args, original, env);
}

void PrimWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void PrimWorker::run()
{
    Worker::run();
    scam::mapEval(args, cont, env);
}
