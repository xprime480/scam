#include "expr/ClosureWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/ClosureBindCont.hpp"
#include "expr/EvalOps.hpp"
#include "util/LambdaDef.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"

using namespace scam;
using namespace std;

ClosureWorker::ClosureWorker(ScamValue closure,
                             Continuation * cont,
                             ScamValue args,
                             Env * argEnv)
    : Worker("proc")
    , closure(closure)
    , cont(cont)
    , args(args)
    , argEnv(argEnv)
{
}

ClosureWorker * ClosureWorker::makeInstance(ScamValue closure,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv)
{
    return new ClosureWorker(closure, cont, args, argEnv);
}

void ClosureWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        closure->mark();
        cont->mark();
        args->mark();
        argEnv->mark();
    }
}

void ClosureWorker::run()
{
    Worker::run();

    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    Continuation * newCont = mm.make<ClosureBindCont>(closure->closureDef(),
                                                      closure->closureEnv(),
                                                      cont);
    mapEval(args, newCont, argEnv);
}
