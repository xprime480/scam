#include "form/AndWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AndCont.hpp"
#include "util/MemoryManager.hpp"

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
