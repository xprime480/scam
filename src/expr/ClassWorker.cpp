#include "expr/ClassWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ClassCont.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ClassWorker::ClassWorker(ScamValue cls,
                         ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine)
    : Worker("ClassWorker", engine)
    , cls(cls)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ClassWorker * ClassWorker::makeInstance(ScamValue cls,
                                        ScamValue args,
                                        Continuation * cont,
                                        Env * env,
                                        ScamEngine * engine)
{
    return new ClassWorker(cls, args, cont, env, engine);
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
        standardMemoryManager.make<ClassCont>(cls, cont, engine);
    mapEval(args, newCont, env, engine);
}
