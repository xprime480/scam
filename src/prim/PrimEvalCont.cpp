#include "prim/PrimEvalCont.hpp"

using namespace scam;
using namespace std;

PrimEvalCont::PrimEvalCont(PrimWorkerData const & data)
    : Continuation("Primitive Eval")
    , data(data.args, data.original, data.env, data.caller)
{
}

PrimEvalCont *
PrimEvalCont::makeInstance(PrimWorkerData const & data)
{
    return new PrimEvalCont(data);
}

void PrimEvalCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}

void PrimEvalCont::run(ScamValue expr)
{
    Continuation::run(expr);
    data.handleResult(expr);
}
