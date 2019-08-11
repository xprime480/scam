#include "expr/ClosureWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ClosureBindCont.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "util/LambdaDef.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ClosureWorker::ClosureWorker(ScamValue closure,
                             Continuation * cont,
                             ScamValue args,
                             Env * argEnv,
                             ScamEngine * engine)
    : Worker("proc", engine)
    , closure(closure)
    , cont(cont)
    , args(args)
    , argEnv(argEnv)
{
}

ClosureWorker * ClosureWorker::makeInstance(ScamValue closure,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv,
                                            ScamEngine * engine)
{
    return new ClosureWorker(closure, cont, args, argEnv, engine);
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

    Continuation * newCont
        = standardMemoryManager.make<ClosureBindCont>(closure->closureDef(),
                                                      closure->closureEnv(),
                                                      cont,
                                                      engine);

    mapEval(args, newCont, argEnv, engine);
}
