#include "expr/ExprEvalCont.hpp"

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

void ExprEvalCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( isError(expr) ) {
        data.original->run(expr);
    }
    else {
        apply(expr, data.cdr, data.original, data.env, engine);
    }
}
