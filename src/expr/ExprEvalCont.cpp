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

void ExprEvalCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isError(value) ) {
        engine->handleError(value);
    }
    else {
        apply(value, data.cdr, data.original, data.env, engine);
    }
}
