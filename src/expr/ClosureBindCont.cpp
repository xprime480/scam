#include "expr/ClosureBindCont.hpp"

#include "Binder.hpp"
#include "Continuation.hpp"
#include "Env.hpp"
#include "EvalWorker.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "expr/MacroEvalCont.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "util/LambdaDef.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ClosureBindCont::ClosureBindCont(LambdaDef & lambda,
                                 Env * capture,
                                 Continuation * cont,
                                 bool macrolike,
                                 ScamEngine * engine)
    : Continuation("proc - bind", engine)
    , lambda(lambda)
    , capture(capture)
    , cont(cont)
    , macrolike(macrolike)
{
}

ClosureBindCont * ClosureBindCont::makeInstance(LambdaDef & lambda,
                                                Env * capture,
                                                Continuation * cont,
                                                bool macrolike,
                                                ScamEngine * engine)
{
    return new ClosureBindCont(lambda, capture, cont, macrolike, engine);
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
        engine->handleError(value);
    }
    else if ( malformedActuals(value) ) {
        /* do nothing */
    }
    else if ( checkArgLength(value) ) {
        finalize(value);
    }
}

bool ClosureBindCont::malformedActuals(ScamValue expr) const
{
    if ( isPair(expr) || isNull(expr) || isSymbol(expr) ) {
        return false;
    }

    ScamValue err =
        makeError("Expecting list or symbol for parameter list (%{0})", expr);
    engine->handleError(err);

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
    engine->handleError(err);
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

void ClosureBindCont::finalize(ScamValue actuals)  const
{
    ScamValue formals = lambda.formals;
    Binder binder(capture);
    Env * extended = binder.bind(formals, lambda.rest, actuals);

    Continuation * c =
        ( macrolike
          ? standardMemoryManager.make<MacroEvalCont>(cont, capture, engine)
          : cont );

    workQueueHelper<EvalWorker>(lambda.forms, extended, c, engine);
}
