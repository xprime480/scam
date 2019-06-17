#include "expr/ClosureWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ClosureBindCont.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "input/LambdaParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ClosureWorker::ClosureWorker(LambdaParser * parser,
                             Env * capture,
                             Continuation * cont,
                             ScamValue args,
                             Env * argEnv,
                             bool macrolike,
                             ScamEngine * engine)
    : Worker("proc", engine)
    , parser(parser)
    , capture(capture)
    , cont(cont)
    , args(args)
    , argEnv(argEnv)
    , macrolike(macrolike)
{
}

ClosureWorker * ClosureWorker::makeInstance(LambdaParser * parser,
                                            Env * capture,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv,
                                            bool macrolike,
                                            ScamEngine * engine)
{
    return new ClosureWorker(parser,
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
        parser->mark();
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
        = standardMemoryManager.make<ClosureBindCont>(parser,
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
