
#include "prim/EvalContinuation.hpp"

using namespace scam;
using namespace std;

EvalContinuation::EvalContinuation(PrimWorkerData const & data)
    : Continuation("Primitive Eval")
    , data(data.args, data.original, data.env, data.caller)
{
}

EvalContinuation *
EvalContinuation::makeInstance(PrimWorkerData const & data)
{
    return new EvalContinuation(data);
}

void EvalContinuation::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}

void EvalContinuation::run(ScamExpr * expr)
{
    Continuation::run(expr);
    data.handleResult(expr);
}
