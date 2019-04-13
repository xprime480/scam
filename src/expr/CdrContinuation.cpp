#include "expr/CdrContinuation.hpp"

#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

CdrContinuation::CdrContinuation(WorkerData const & data)
    : Continuation("Cons Map Cdr")
    , data(data)
{
}

CdrContinuation * CdrContinuation::makeInstance(WorkerData const & data)
{
    return new CdrContinuation(data);
}

void CdrContinuation::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}

void CdrContinuation::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        data.original->run(expr);
    }
    else {
        ScamExpr * e = ExpressionFactory::makeCons(data.car, expr);
        data.original->run(e);
    }
}
