#include "form/CondTestApplyCont.hpp"

#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "form/CondApplyCont.hpp"
#include "form/CondWorker.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"

using namespace scam;

CondTestApplyCont::CondTestApplyCont(ScamValue form,
                                     ScamValue clauses,
                                     Continuation * cont,
                                     Env * env)
    : Continuation("Cond Test Apply")
    , form(form)
    , clauses(clauses)
    , cont(cont)
    , env(env)
{
}

CondTestApplyCont * CondTestApplyCont::makeInstance(ScamValue form,
                                                    ScamValue clauses,
                                                    Continuation * cont,
                                                    Env * env)
{
    return new CondTestApplyCont(form, clauses, cont, env);
}

void CondTestApplyCont::mark()
{
    form->mark();
    clauses->mark();
    cont->mark();
    env->mark();
}

void CondTestApplyCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    ScamEngine & engine = ScamEngine::getEngine();
    MemoryManager & mm = engine.getMemoryManager();

    if ( isUnhandledError(value) ) {
        engine.handleError(value);
    }
    else if ( truth(value) ) {
        Continuation * c = mm.make<CondApplyCont>(value, cont, env);
        eval(form, c, env);
    }
    else {
        workQueueHelper<CondWorker>(clauses, cont, env);
    }
}
