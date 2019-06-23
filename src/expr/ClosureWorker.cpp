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
                             bool macrolike,
                             ScamEngine * engine)
    : Worker("proc", engine)
    , lambda(lambda)
    , capture(capture)
    , cont(cont)
    , args(args)
    , argEnv(argEnv)
    , macrolike(macrolike)
{
}

ClosureWorker * ClosureWorker::makeInstance(LambdaDef & lambda,
                                            Env * capture,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv,
                                            bool macrolike,
                                            ScamEngine * engine)
{
    return new ClosureWorker(lambda,
                             capture,
                             cont,
                             args,
                             argEnv,
                             macrolike,
                             engine);
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
                                                      macrolike,
                                                      engine);
    if ( macrolike ) {
        newCont->handleValue(args);
    }
    else {
        mapEval(args, newCont, argEnv, engine);
    }
}
