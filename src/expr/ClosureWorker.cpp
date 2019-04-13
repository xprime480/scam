#include "expr/ClosureWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ClosureBindCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ClosureWorker::ClosureWorker(ScamExpr *formals,
                             ScamExpr * forms,
                             Env * capture,
                             Continuation * cont,
                             ScamExpr * args,
                             Env * argEnv,
                             bool macrolike)
    : Worker("proc")
    , formals(formals)
    , forms(forms)
    , capture(capture)
    , cont(cont)
    , args(args)
    , argEnv(argEnv)
    , macrolike(macrolike)
{
}

ClosureWorker * ClosureWorker::makeInstance(ScamExpr *formals,
                                            ScamExpr * forms,
                                            Env * capture,
                                            Continuation * cont,
                                            ScamExpr * args,
                                            Env * argEnv,
                                            bool macrolike)
{
    return new ClosureWorker(formals,
                             forms,
                             capture,
                             cont,
                             args,
                             argEnv,
                             macrolike);
}

void ClosureWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        formals->mark();
        forms->mark();
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
        = standardMemoryManager.make<ClosureBindCont>(formals,
                                                      forms,
                                                      capture,
                                                      cont,
                                                      macrolike);
    if ( macrolike ) {
        newCont->run(args);
    }
    else {
        args->mapEval(newCont, argEnv);
    }
}
