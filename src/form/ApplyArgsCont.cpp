#include "form/ApplyArgsCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ApplyArgsCont::ApplyArgsCont(ScamValue op, Continuation * cont, Env * env)
    : Continuation("Apply Args")
    , op(op)
    , cont(cont)
    , env(env)
{
}

ApplyArgsCont *
ApplyArgsCont::makeInstance(ScamValue op, Continuation * cont, Env * env)
{
    return new ApplyArgsCont(op, cont, env);
}

void ApplyArgsCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        op->mark();
        cont->mark();
        env->mark();
    }
}

void ApplyArgsCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        apply(op, value, cont, env);
    }
}
