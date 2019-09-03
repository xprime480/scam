#include "form/AssignWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "form/AssignCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AssignWorker::AssignWorker(ScamValue sym,
                           ScamValue value,
                           Continuation * cont,
                           Env * env)
    : Worker("Assign")
    , sym(sym)
    , value(value)
    , cont(cont)
    , env(env)
{
}

AssignWorker * AssignWorker::makeInstance(ScamValue sym,
                                           ScamValue value,
                                           Continuation * cont,
                                           Env * env)
{
    return new AssignWorker(sym, value, cont, env);
}

void AssignWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        sym->mark();
        value->mark();
        cont->mark();
        env->mark();
    }
}

void AssignWorker::run()
{
    Worker::run();

    Continuation * c = standardMemoryManager.make<AssignCont>(sym, cont, env);
    eval(value, c, env);
}
