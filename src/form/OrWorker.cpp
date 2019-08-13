#include "form/OrWorker.hpp"

#include "Continuation.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/OrCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

OrWorker::OrWorker(Continuation * cont,
                   Env * env,
                   ScamValue args,
                   ScamEngine * engine)
    : Worker("Or", engine)
    , args(args)
    , cont(cont)
    , env(env)
{
}

OrWorker * OrWorker::makeInstance(Continuation * cont,
                                  Env * env,
                                  ScamValue args,
                                  ScamEngine * engine)
{
    return new OrWorker(cont, env, args, engine);
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
        Continuation * newCont =
            standardMemoryManager.make<OrCont>(rest, cont, env, engine);
        eval(test, newCont, env, engine);
    }
}
