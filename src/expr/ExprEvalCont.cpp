#include "expr/ExprEvalCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ExprEvalCont::ExprEvalCont(ScamValue cdr,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
    : Continuation("Cons Eval Eval", engine)
    , cdr(cdr)
    , cont(cont)
    , env(env)
{
}

ExprEvalCont * ExprEvalCont::makeInstance(ScamValue cdr,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine)
{
    return new ExprEvalCont(cdr, cont, env, engine);
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
        engine->handleError(value);
    }
    else {
        apply(value, cdr, cont, env, engine);
    }
}
