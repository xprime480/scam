#include "form/EvalCont.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

EvalCont::EvalCont(Continuation * cont, Env * env, ScamEngine * engine)
    : Continuation("eval", engine)
    , cont(cont)
    , env(env)
{
}

EvalCont *
EvalCont::makeInstance(Continuation * cont, Env * env, ScamEngine * engine)
{
    return new EvalCont(cont, env, engine);
}

void EvalCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        cont->mark();
        env->mark();
    }
}

void EvalCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        engine->handleError(value);
    }
    else {
        eval(value, cont, engine->getInteractionFrame(), engine);
    }
}
