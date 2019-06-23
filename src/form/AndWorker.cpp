#include "form/AndWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AndCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AndWorker::AndWorker(Continuation * cont,
                     Env * env,
                     ScamValue args,
                     ScamEngine * engine)
    : Worker("And", engine)
    , cont(cont)
    , env(env)
    , args(args)
{
}

AndWorker * AndWorker::makeInstance(Continuation * cont,
                                    Env * env,
                                    ScamValue args,
                                    ScamEngine * engine)
{
    return new AndWorker(cont, env, args, engine);
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
        Continuation * newCont =
            standardMemoryManager.make<AndCont>(rest, cont, env, engine);
        eval(test, newCont, env, engine);
    }
}
