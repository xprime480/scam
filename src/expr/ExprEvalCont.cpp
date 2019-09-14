#include "expr/ExprEvalCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"

using namespace scam;
using namespace std;

ExprEvalCont::ExprEvalCont(ScamValue cdr, Continuation * cont, Env * env)
    : Continuation("Cons Eval Eval")
    , cdr(cdr)
    , cont(cont)
    , env(env)
{
}

ExprEvalCont *
ExprEvalCont::makeInstance(ScamValue cdr, Continuation * cont, Env * env)
{
    return new ExprEvalCont(cdr, cont, env);
}

void ExprEvalCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cdr->mark();
        cont->mark();
        env->mark();
    }
}

void ExprEvalCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        apply(value, cdr, cont, env);
    }
}
