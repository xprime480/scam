#include "expr/ExprEvalCont.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ExprEvalCont::ExprEvalCont(WorkerData const & data)
    : Continuation("Cons Eval Eval")
    , data(data)
{
}

ExprEvalCont * ExprEvalCont::makeInstance(WorkerData const & data)
{
    return new ExprEvalCont(data);
}

void ExprEvalCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}

void ExprEvalCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        data.original->run(expr);
    }
    else {
        expr->apply(data.cdr, data.original, data.env);
    }
}
