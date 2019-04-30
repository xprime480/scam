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

ClosureBindCont::ClosureBindCont(const LambdaParser * lambda,
                                 Env * capture,
                                 Continuation * cont,
                                 bool macrolike)
    : Continuation("proc - bind")
    , lambda(lambda)
    , capture(capture)
    , cont(cont)
    , macrolike(macrolike)
{
}

ClosureBindCont * ClosureBindCont::makeInstance(const LambdaParser * lambda,
                                                Env * capture,
                                                Continuation * cont,
                                                bool macrolike)
{
    return new ClosureBindCont(lambda, capture, cont, macrolike);
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

void ClosureBindCont::run(ExprHandle expr)
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

bool ClosureBindCont::malformedActuals(ExprHandle expr) const
{
    if ( expr->isCons() || expr->isNil() || expr->isSymbol() ) {
        return false;
    }

    ExprHandle err =
        ExpressionFactory::makeError( "Expected a paramter list, got: ",
                                      expr->toString());
    cont->run(err);

    return true;
}

bool ClosureBindCont::describeFormals(unsigned & len) const
{
    const ParameterListParser * formals = lambda->getArgs();
    const ScamSymbol * rest = formals->getRest();

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
    ExprHandle err =
        ExpressionFactory::makeError("Expected ",
                                     formalsLen,
                                     " parameters; ",
                                     "got ",
                                     actualsLen);
    cont->run(err);
}

bool ClosureBindCont::checkArgLength(ExprHandle expr) const
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

void ClosureBindCont::finalize(ExprHandle actuals)  const
{
    ExprHandle formals = lambda->getArgs()->getValue();
    Binder binder(capture);
    Env * extended = binder.bind(formals, actuals);

    Continuation * c =
        ( macrolike
          ? standardMemoryManager.make<MacroEvalCont>(cont, capture)
          : cont );

    ExprHandle forms = lambda->getFormList();
    workQueueHelper<EvalWorker>(forms, extended, c);
}
