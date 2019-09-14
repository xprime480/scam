#include "form/IfWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "form/IFCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"

using namespace scam;
using namespace std;

IfWorker::IfWorker(Continuation * cont, Env * env, ScamValue args)
    : Worker("If")
    , args(args)
    , cont(cont)
    , env(env)
{
}

IfWorker *
IfWorker::makeInstance(Continuation * cont, Env * env, ScamValue args)
{
    return new IfWorker(cont, env, args);
}

void IfWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void IfWorker::run()
{
    Worker::run();

    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    Continuation * c = mm.make<IfCont>(args, cont, env);
    ScamValue test = nthcar(args, 0);
    eval(test, c, env);
}
