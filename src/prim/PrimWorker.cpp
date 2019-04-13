#include "prim/PrimWorker.hpp"

#include "prim/PrimEvalCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

PrimWorker::PrimWorker(Continuation * cont,
                       Env * env,
                       ScamExpr * args,
                       Primitive * caller)
    : Worker("Primitive")
    , data(args, cont, env, caller)
{
    data.cont = standardMemoryManager.make<PrimEvalCont>(data);
}

PrimWorker * PrimWorker::makeInstance(Continuation * cont,
                                      Env * env,
                                      ScamExpr * args,
                                      Primitive * caller)
{
    return new PrimWorker(cont, env, args, caller);
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
    data.mapEval();
}

