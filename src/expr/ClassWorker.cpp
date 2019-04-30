#include "expr/ClassWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ClassCont.hpp"
#include "expr/ScamClass.hpp"
#include "expr/ScamExpr.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ClassWorker::ClassWorker(const ScamClass * cls,
                         ExprHandle args,
                         Continuation * cont,
                         Env * env)
    : Worker("ClassWorker")
    , cls(cls)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ClassWorker * ClassWorker::makeInstance(const ScamClass * cls,
                                        ExprHandle args,
                                        Continuation * cont,
                                        Env * env)
{
    return new ClassWorker(cls, args, cont, env);
}

void ClassWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        cls->mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void ClassWorker::run()
{
    Worker::run();
    Continuation * newCont =
        standardMemoryManager.make<ClassCont>(cls, cont);
    args->mapEval(newCont, env);
}
