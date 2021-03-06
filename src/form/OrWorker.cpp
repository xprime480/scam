#include "form/OrWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "form/OrCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

OrWorker::OrWorker(Continuation * cont, Env * env, ScamValue args)
    : Worker("Or")
    , args(args)
    , cont(cont)
    , env(env)
{
}

OrWorker *
OrWorker::makeInstance(Continuation * cont, Env * env, ScamValue args)
{
    return new OrWorker(cont, env, args);
}

void OrWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void OrWorker::run()
{
    Worker::run();

    if ( isNull(args) ) {
        cont->handleValue(makeBoolean(false));
    }
    else {
        ScamValue test = getCar(args);
        ScamValue rest = getCdr(args);
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * newCont = mm.make<OrCont>(rest, cont, env);
        eval(test, newCont, env);
    }
}
