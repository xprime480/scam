#include "expr/ClosureBindCont.hpp"

#include "Binder.hpp"
#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "EvalWorker.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/SequenceOps.hpp"
#include "util/LambdaDef.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

ClosureBindCont::ClosureBindCont(LambdaDef & lambda,
                                 Env * capture,
                                 Continuation * cont)
    : Continuation("proc - bind")
    , lambda(lambda)
    , capture(capture)
    , cont(cont)
{
}

ClosureBindCont * ClosureBindCont::makeInstance(LambdaDef & lambda,
                                                Env * capture,
                                                Continuation * cont)
{
    return new ClosureBindCont(lambda, capture, cont);
}

void ClosureBindCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        lambda.mark();
        capture->mark();
        cont->mark();
    }
}

void ClosureBindCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else if ( malformedActuals(value) ) {
        /* do nothing */
    }
    else if ( checkArgLength(value) ) {
        ScamValue test = finalize(value);
        if ( isError(test) ) {
            ScamEngine::getEngine().handleError(test);
        }
    }
}

bool ClosureBindCont::malformedActuals(ScamValue expr) const
{
    if ( isPair(expr) || isNull(expr) || isSymbol(expr) ) {
        return false;
    }

    ScamValue err =
        makeError("Expecting list or symbol for parameter list (%{0})", expr);
    err->errorCategory() = argsCategory;
    ScamEngine::getEngine().handleError(err);

    return true;
}

bool ClosureBindCont::describeFormals(unsigned & len) const
{
    len = length(lambda.formals);
    return ! isNothing(lambda.rest);
}

void ClosureBindCont::wrongNumberOfParameters(unsigned formalsLen,
                                              unsigned actualsLen) const
{
    const char * cause;
    if ( formalsLen > actualsLen ) {
        cause = "Too few parameters (%{1} of %{0})";
    }
    else {
        cause = "Too many parameters (%{1} of %{0})";
    }

    ScamValue err = makeError(cause,
                              makeInteger(formalsLen, true),
                              makeInteger(actualsLen, true));
    err->errorCategory() = argsCategory;
    ScamEngine::getEngine().handleError(err);
}

bool ClosureBindCont::checkArgLength(ScamValue expr) const
{
    unsigned exp { 0 };
    bool optFinal = describeFormals(exp);
    unsigned act = length(expr);

    if ( (act < exp) || ((! optFinal) && (act > exp)) ) {
        wrongNumberOfParameters(exp, act);
        return false;
    }

    return true;
}

ScamValue ClosureBindCont::finalize(ScamValue actuals) const
{
    ScamValue formals = lambda.formals;
    Binder binder(capture);

    ScamValue test = binder.bind(formals, lambda.rest, actuals);
    if ( isError(test) ) {
        return test;
    }

    Env * extended = test->envValue();
    workQueueHelper<EvalWorker>(lambda.forms, extended, cont);

    return makeNothing();
}
