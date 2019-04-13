#include "expr/ClassInitWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ClassInitCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

namespace
{
    static ScamExpr * const initSym =
        ExpressionFactory::makeSymbol("init", false);
}

ClassInitWorker::ClassInitWorker(ScamExpr * instance,
                                 ScamExpr * args,
                                 Continuation * cont,
                                 Env * env)
    : Worker("ClassInit")
    , instance(instance)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ClassInitWorker * ClassInitWorker::makeInstance(ScamExpr * instance,
                                                ScamExpr * args,
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
    ScamExpr * newArgs
        = ExpressionFactory::makeCons(initSym, args);

    instance->apply(newArgs, newCont, env);
}
