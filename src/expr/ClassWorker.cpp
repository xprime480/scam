#include "expr/ClassWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/ClassCont.hpp"
#include "expr/EvalOps.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"

using namespace scam;
using namespace std;

ClassWorker::ClassWorker(ScamValue cls,
                         ScamValue args,
                         Continuation * cont,
                         Env * env)
    : Worker("ClassWorker")
    , cls(cls)
    , args(args)
    , cont(cont)
    , env(env)
{
}

ClassWorker * ClassWorker::makeInstance(ScamValue cls,
                                        ScamValue args,
                                        Continuation * cont,
                                        Env * env)
{
    return new ClassWorker(cls, args, cont, env);
}

void ClassWorker::mark()
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

    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    Continuation * newCont = mm.make<ClassCont>(cls, cont);
    mapEval(args, newCont, env);
}
