#include "expr/ClosureWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ClosureBindCont.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamExpr.hpp"
#include "input/LambdaParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ClosureWorker::ClosureWorker(const LambdaParser * parser,
                             Env * capture,
                             Continuation * cont,
                             ScamValue args,
                             Env * argEnv,
                             bool macrolike)
    : Worker("proc")
    , parser(parser)
    , capture(capture)
    , cont(cont)
    , args(args)
    , argEnv(argEnv)
    , macrolike(macrolike)
{
}

ClosureWorker * ClosureWorker::makeInstance(const LambdaParser * parser,
                                            Env * capture,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv,
                                            bool macrolike)
{
    return new ClosureWorker(parser, capture, cont, args, argEnv, macrolike);
}

void ClosureWorker::mark() const
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
                                                      macrolike);
    if ( macrolike ) {
        newCont->run(args);
    }
    else {
        mapEval(args, newCont, argEnv);
    }
}
