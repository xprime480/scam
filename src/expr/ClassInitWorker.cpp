#include "expr/ClassInitWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ClassInitCont.hpp"
#include "expr/ClassOps.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
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
                                 Env * env,
                                 ScamEngine * engine)
    : Worker("ClassInit", engine)
    , instance(instance)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ClassInitWorker * ClassInitWorker::makeInstance(ScamValue instance,
                                                ScamValue args,
                                                Continuation * cont,
                                                Env * env,
                                                ScamEngine * engine)
{
    return new ClassInitWorker(instance, args, cont, env, engine);
}

void ClassInitWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void ClassInitWorker::run()
{
    Worker::run();

    Env * env = getInstanceFunctionMap(instance);
    if ( ! env->check(initSym) ) {
        cont->handleValue(instance);
        return;
    }

    Continuation * newCont
        = standardMemoryManager.make<ClassInitCont>(instance, cont, engine);
    ScamValue newArgs = makePair(initSym, args);
    apply(instance, newArgs, newCont, env, engine);
}
