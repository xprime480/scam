#include "form/AndWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "form/AndCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

AndWorker::AndWorker(Continuation * cont, Env * env, ScamValue args)
    : Worker("And")
    , cont(cont)
    , env(env)
    , args(args)
{
}

AndWorker *
AndWorker::makeInstance(Continuation * cont, Env * env, ScamValue args)
{
    return new AndWorker(cont, env, args);
}

void AndWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        cont->mark();
        env->mark();
        args->mark();
    }
}

void AndWorker::run()
{
    Worker::run();

    if ( isNull(args) ) {
        cont->handleValue(makeBoolean(true));
    }
    else {
        ScamValue test = getCar(args);
        ScamValue rest = getCdr(args);
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * newCont = mm.make<AndCont>(rest, cont, env);
        eval(test, newCont, env);
    }
}
