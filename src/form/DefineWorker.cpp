#include "form/DefineWorker.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "form/DefineCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"

using namespace scam;
using namespace std;

DefineWorker::DefineWorker(ScamValue symbol,
                           ScamValue form,
                           Continuation * cont,
                           Env * env)
    : Worker("Define")
    , symbol(symbol)
    , form(form)
    , cont(cont)
    , env(env)
{
}

DefineWorker * DefineWorker::makeInstance(ScamValue symbol,
                                          ScamValue form,
                                          Continuation * cont,
                                          Env * env)
{
    return new DefineWorker(symbol, form, cont, env);
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

    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    Continuation * c = mm.make<DefineCont>(symbol, cont, env);
    eval(form, c, env);
}
