#include "expr/ClosureBindCont.hpp"

#include "Binder.hpp"
#include "Continuation.hpp"
#include "Env.hpp"
#include "EvalWorker.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/MacroEvalCont.hpp"
#include "expr/ScamExpr.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ClosureBindCont::ClosureBindCont(ScamExpr * formals,
                ScamExpr * forms,
                Env * capture,
                Continuation * cont,
                bool macrolike)
    : Continuation("proc - bind")
    , formals(formals)
    , forms(forms)
    , capture(capture)
    , cont(cont)
    , macrolike(macrolike)
{
}

ClosureBindCont * ClosureBindCont::makeInstance(ScamExpr * formals,
                               ScamExpr * forms,
                               Env * capture,
                               Continuation * cont,
                               bool macrolike)
{
    return new ClosureBindCont(formals,
                               forms,
                               capture,
                               cont,
                               macrolike);
}

void ClosureBindCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        formals->mark();
        forms->mark();
        capture->mark();
        cont->mark();
    }
}

void ClosureBindCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( malformedActuals(expr) ) {
        /* do  nothing */
    }
    else if ( checkArgLength(expr) ) {
        finalize(expr);
    }
}

bool ClosureBindCont::malformedActuals(ScamExpr * expr) const
{
    if ( expr->isCons() || expr->isNil() || expr->isSymbol() ) {
        return false;
    }

    ScamExpr * err =
        ExpressionFactory::makeError( "Expected a paramter list, got: ",
                                      expr->toString());
    cont->run(err);

    return true;
}

bool ClosureBindCont::describeFormals(unsigned & len) const
{
    if ( formals->isSymbol() ) {
        len = 0;
        return true;
    }

    len = formals->length();
    if ( ! formals->isList() ) {
        --len;
        return true;
    }
    return false;
}

void ClosureBindCont::wrongNumberOfParameters(unsigned formalsLen,
                                              unsigned actualsLen) const
{
    ScamExpr * err =
        ExpressionFactory::makeError("Expected ",
                                     formalsLen,
                                     " parameters; ",
                                     "got ",
                                     actualsLen);
    cont->run(err);
}

bool ClosureBindCont::checkArgLength(ScamExpr * expr) const
{
    unsigned exp { 0 };
    bool optFinal = describeFormals(exp);
    unsigned act = expr->length();

    if ( (act < exp) || ((! optFinal) && (act > exp)) ) {
        wrongNumberOfParameters(exp, act);
        return false;
    }

    return true;
}

void ClosureBindCont::finalize(ScamExpr * actuals)  const
{
    Binder binder(capture);
    Env * extended = binder.bind(formals, actuals);

    Continuation * c =
        ( macrolike
          ? standardMemoryManager.make<MacroEvalCont>(cont, capture)
          : cont );

    workQueueHelper<EvalWorker>(forms, extended, c);
}
