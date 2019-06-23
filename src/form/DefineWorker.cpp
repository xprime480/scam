#include "form/DefineWorker.hpp"

#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "form/DefineCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

DefineWorker::DefineWorker(ScamValue symbol,
                           ScamValue form,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
    : Worker("Define", engine)
    , symbol(symbol)
    , form(form)
    , cont(cont)
    , env(env)
{
}

DefineWorker * DefineWorker::makeInstance(ScamValue symbol,
                                          ScamValue form,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine)
{
    return new DefineWorker(symbol, form, cont, env, engine);
}

void DefineWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        symbol->mark();
        form->mark();
        cont->mark();
        env->mark();
    }
}

void DefineWorker::run()
{
    Worker::run();

    Continuation * c =
        standardMemoryManager.make<DefineCont>(symbol, cont, env, engine);
    eval(form, c, env, engine);
}
