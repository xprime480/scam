#include "expr/ExprEvalCont.hpp"

#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ExprEvalCont::ExprEvalCont(WorkerData const & data, ScamEngine * engine)
    : Continuation("Cons Eval Eval", engine)
    , data(data)
{
}

ExprEvalCont *
ExprEvalCont::makeInstance(WorkerData const & data, ScamEngine * engine)
{
    return new ExprEvalCont(data, engine);
}

void ExprEvalCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        data.mark();
    }
}

void ExprEvalCont::handleValue(ScamValue expr)
{
    Continuation::handleValue(expr);

    if ( isError(expr) ) {
        engine->handleError(expr);
    }
    else {
        apply(expr, data.cdr, data.original, data.env, engine);
    }
}
