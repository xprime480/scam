#include "expr/ClassInitWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/ClassInitCont.hpp"
#include "expr/ClassOps.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

namespace
{
    static ScamValue const initSym = makeSymbol("init", false);
}

ClassInitWorker::ClassInitWorker(ScamValue instance,
                                 ScamValue args,
                                 Continuation * cont,
                                 Env * env)
    : Worker("ClassInit")
    , instance(instance)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ClassInitWorker * ClassInitWorker::makeInstance(ScamValue instance,
                                                ScamValue args,
                                                Continuation * cont,
                                                Env * env)
{
    return new ClassInitWorker(instance, args, cont, env);
}

void ClassInitWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        instance->mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void ClassInitWorker::run()
{
    Worker::run();

    Env * env = getInstanceFunctionMap(instance);

    ScamValue test = env->check(initSym);
    if ( isUnhandledError(test) ) {
        ScamEngine::getEngine().handleError(test);
    }
    else if ( ! truth(test) ) {
        cont->handleValue(instance);
    }
    else {
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * newCont = mm.make<ClassInitCont>(instance, cont);
        ScamValue newArgs = makePair(initSym, args);
        apply(instance, newArgs, newCont, env);
    }
}
