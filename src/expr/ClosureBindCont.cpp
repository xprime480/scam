#include "expr/ClosureBindCont.hpp"

#include "Binder.hpp"
#include "Continuation.hpp"
#include "Env.hpp"
#include "EvalWorker.hpp"
#include "WorkQueue.hpp"
#include "expr/MacroEvalCont.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "input/LambdaParser.hpp"
#include "input/ParameterListParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ClosureBindCont::ClosureBindCont(const LambdaParser * lambda,
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

ClosureBindCont * ClosureBindCont::makeInstance(const LambdaParser * lambda,
                                                Env * capture,
                                                Continuation * cont,
                                                bool macrolike,
                                                ScamEngine * engine)
{
    return new ClosureBindCont(lambda, capture, cont, macrolike, engine);
}

void ClosureBindCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        lambda->mark();
        capture->mark();
        cont->mark();
    }
}

void ClosureBindCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( isError(expr) ) {
        cont->run(expr);
    }
    else if ( malformedActuals(expr) ) {
        /* do nothing */
    }
    else if ( checkArgLength(expr) ) {
        finalize(expr);
    }
}

bool ClosureBindCont::malformedActuals(ScamValue expr) const
{
    if ( isPair(expr) || isNull(expr) || isSymbol(expr) ) {
        return false;
    }

    ScamValue err =
        makeErrorExtended( "Expected a paramter list, got: ", writeValue(expr));
    cont->run(err);

    return true;
}

bool ClosureBindCont::describeFormals(unsigned & len) const
{
    const ParameterListParser * formals = lambda->getArgs();
    ScamValue rest = formals->getRest();

    len = formals->size();
    if ( nullptr != rest ) {
        --len;
        return true;
    }

    return false;
}

void ClosureBindCont::wrongNumberOfParameters(unsigned formalsLen,
                                              unsigned actualsLen) const
{
    ScamValue err = makeErrorExtended("Expected ",
                                      formalsLen,
                                      " parameters; got ",
                                      actualsLen);
    cont->run(err);
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
    ScamValue formals = lambda->getArgs()->getValue();
    Binder binder(capture);
    Env * extended = binder.bind(formals, actuals);

    Continuation * c =
        ( macrolike
          ? standardMemoryManager.make<MacroEvalCont>(cont, capture, engine)
          : cont );

    ScamValue forms = lambda->getFormList();
    workQueueHelper<EvalWorker>(forms, extended, c, engine);
}
