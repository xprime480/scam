#include "prim/PrimWorker.hpp"

#include "prim/PrimEvalCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

PrimWorker::PrimWorker(Continuation * cont,
                       Env * env,
                       ScamEngine * engine,
                       ScamValue args,
                       ScamValue caller)
    : Worker("Primitive", engine)
    , data(args, cont, env, caller)
{
    data.cont = standardMemoryManager.make<PrimEvalCont>(data, engine);
}

PrimWorker * PrimWorker::makeInstance(Continuation * cont,
                                      Env * env,
                                      ScamEngine * engine,
                                      ScamValue args,
                                      ScamValue caller)
{
    return new PrimWorker(cont, env, engine, args, caller);
}

void PrimWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        data.mark();
    }
}

void PrimWorker::run()
{
    Worker::run();
    data.mapEval(engine);
}
