#include "expr/ClassInitWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ClassInitCont.hpp"
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

    Continuation * newCont
        = standardMemoryManager.make<ClassInitCont>(instance, cont);
    ScamValue newArgs = makeCons(initSym, args);
    apply(instance, newArgs, newCont, env);
}
