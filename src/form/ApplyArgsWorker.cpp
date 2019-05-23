#include "form/ApplyArgsWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "form/ApplyArgsCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ApplyArgsWorker::ApplyArgsWorker(ScamValue op,
                                 ScamValue args,
                                 Continuation * cont,
                                 Env * env)
    : Worker("Apply Args")
    , op(op)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ApplyArgsWorker * ApplyArgsWorker::makeInstance(ScamValue op,
                                                ScamValue args,
                                                Continuation * cont,
                                                Env * env)
{
    return new ApplyArgsWorker(op, args, cont, env);
}

void ApplyArgsWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        op->mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void ApplyArgsWorker::run()
{
    Continuation * newCont =
        standardMemoryManager.make<ApplyArgsCont>(op, cont, env);
    args->eval(newCont, env);
}
