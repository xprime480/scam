#include "form/ApplyArgsWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "form/ApplyArgsCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ApplyArgsWorker::ApplyArgsWorker(ScamValue op,
                                 ScamValue args,
                                 Continuation * cont,
                                 Env * env,
                                 ScamEngine * engine)
    : Worker("Apply Args", engine)
    , op(op)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ApplyArgsWorker * ApplyArgsWorker::makeInstance(ScamValue op,
                                                ScamValue args,
                                                Continuation * cont,
                                                Env * env,
                                                ScamEngine * engine)
{
    return new ApplyArgsWorker(op, args, cont, env, engine);
}

void ApplyArgsWorker::mark()
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
        standardMemoryManager.make<ApplyArgsCont>(op, cont, env, engine);
    eval(args, newCont, env, engine);
}
