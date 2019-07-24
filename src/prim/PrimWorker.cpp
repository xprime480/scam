#include "prim/PrimWorker.hpp"

#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "prim/PrimEvalCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

PrimWorker::PrimWorker(ScamValue caller,
                       ScamValue args,
                       Continuation * original,
                       Env * env,
                       ScamEngine * engine)
    : Worker("Primitive", engine)
    , args(args)
    , env(env)
{
    cont = standardMemoryManager.make<PrimEvalCont>(caller, original, engine);
}

PrimWorker * PrimWorker::makeInstance(ScamValue caller,
                                      ScamValue args,
                                      Continuation * original,
                                      Env * env,
                                      ScamEngine * engine)
{
    return new PrimWorker(caller, args, original, env, engine);
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
    scam::mapEval(args, cont, env, engine);
}
