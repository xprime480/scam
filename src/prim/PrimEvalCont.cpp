#include "prim/PrimEvalCont.hpp"

using namespace scam;
using namespace std;

PrimEvalCont::PrimEvalCont(PrimWorkerData const & data, ScamEngine * engine)
    : Continuation("Primitive Eval", engine)
    , data(data.args, data.original, data.env, data.caller)
{
}

PrimEvalCont *
PrimEvalCont::makeInstance(PrimWorkerData const & data, ScamEngine * engine)
{
    return new PrimEvalCont(data, engine);
}

void PrimEvalCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}

void PrimEvalCont::handleValue(ScamValue expr)
{
    Continuation::handleValue(expr);
    data.handleResult(expr, engine);
}
