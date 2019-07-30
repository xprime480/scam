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

ClosureWorker::ClosureWorker(LambdaDef & lambda,
                             Env * capture,
                             Continuation * cont,
                             ScamValue args,
                             Env * argEnv,
                             ScamEngine * engine)
    : Worker("proc", engine)
    , lambda(lambda)
    , capture(capture)
    , cont(cont)
    , args(args)
    , argEnv(argEnv)
{
}

ClosureWorker * ClosureWorker::makeInstance(LambdaDef & lambda,
                                            Env * capture,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv,
                                            ScamEngine * engine)
{
    return new ClosureWorker(lambda, capture, cont, args, argEnv, engine);
}

void ClosureWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        lambda.mark();
        capture->mark();
        cont->mark();
        args->mark();
        argEnv->mark();
    }
}

void ClosureWorker::run()
{
    Worker::run();

    Continuation * newCont
        = standardMemoryManager.make<ClosureBindCont>(lambda,
                                                      capture,
                                                      cont,
                                                      engine);
    mapEval(args, newCont, argEnv, engine);
}
