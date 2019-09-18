#include "form/CondWorker.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EqualityOps.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "form/CondTestApplyCont.hpp"
#include "form/CondTestCont.hpp"
#include "form/ExtractLastCont.hpp"
#include "util/MemoryManager.hpp"
#include "util/Parameter.hpp"
#include "value/ScamData.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;

CondWorker::CondWorker(ScamValue clauses, Continuation * cont, Env * env)
    : Worker("Cond")
    , clauses(clauses)
    , cont(cont)
    , env(env)
{
}

CondWorker *
CondWorker::makeInstance(ScamValue clauses, Continuation * cont, Env * env)
{
    return new CondWorker(clauses, cont, env);
}

void CondWorker::mark()
{
    clauses->mark();
    cont->mark();
    env->mark();
}

void CondWorker::run()
{
    Worker::run();

    ScamEngine & engine = ScamEngine::getEngine();
    MemoryManager & mm = engine.getMemoryManager();

    if ( isNull(clauses) ) {
        cont->handleValue(makeNull());
        return;
    }

    ScamValue clause = getCar(clauses);
    ScamValue rest   = getCdr(clauses);

    ObjectParameter  pObj;
    CountedParameter p0(pObj, 1);
    if ( argsToParms(clause, "Cond", p0) ) {
        ScamValue value = getCar(p0.value);
        ScamValue forms = getCdr(p0.value);

        if ( equals(value, makeSymbol("else")) ) {
            if ( ! checkForNullElse(forms) ) {
                Continuation * c = mm.make<ExtractLastCont>(cont);
                mapEval(forms, c, env);
            }
        }
        else if ( ! isNull(forms) && equals(getCar(forms), makeSymbol("=>")) ) {
            forms = getCdr(forms);
            if ( ! checkApplyForms(forms) )  {
                ScamValue form = getCar(forms);
                Continuation * c =
                    mm.make<CondTestApplyCont>(form, rest, cont, env);
                eval(value, c, env);
            }
        }
        else {
            Continuation * c = mm.make<CondTestCont>(forms, rest, cont, env);
            eval(value, c, env);
        }
    }
}

bool CondWorker::checkForNullElse(ScamValue clauses)
{
    if ( isNull(clauses) ) {
        ScamValue err =
            makeError("else clause requires at least one result form");
        err->errorCategory() = syntaxCategory;
        ScamEngine::getEngine().handleError(err);
        return true;
    }

    return false;
}

bool CondWorker::checkApplyForms(ScamValue clauses)
{
    if ( 1 != length(clauses) ) {
        ScamValue err =
            makeError("=> clause requires exactly one result form",
                      clauses);
        err->errorCategory() = syntaxCategory;
        ScamEngine::getEngine().handleError(err);
        return true;
    }

    return false;
}
