#include "form/CondTestCont.hpp"

#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "form/CondWorker.hpp"
#include "form/ExtractLastCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"

using namespace scam;

CondTestCont::CondTestCont(ScamValue forms,
                           ScamValue clauses,
                           Continuation * cont,
                           Env * env)
    : Continuation("Cond Test")
    , forms(forms)
    , clauses(clauses)
    , cont(cont)
    , env(env)
{
}

CondTestCont * CondTestCont::makeInstance(ScamValue forms,
                                          ScamValue clauses,
                                          Continuation * cont,
                                          Env * env)
{
    return new CondTestCont(forms, clauses, cont, env);
}

void CondTestCont::mark()
{
    forms->mark();
    clauses->mark();
    cont->mark();
    env->mark();
}

void CondTestCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    ScamEngine & engine = ScamEngine::getEngine();
    MemoryManager & mm = engine.getMemoryManager();

    if ( isUnhandledError(value) ) {
        engine.handleError(value);
    }
    else if ( truth(value) ) {
        if ( isNull(forms) ) {
            cont->handleValue(value);
        }
        else {
            Continuation * c = mm.make<ExtractLastCont>(cont);
            mapEval(forms, c, env);
        }
    }
    else {
        workQueueHelper<CondWorker>(clauses, cont, env);
    }
}

