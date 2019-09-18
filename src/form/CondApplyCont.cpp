#include "form/CondApplyCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;

CondApplyCont::CondApplyCont(ScamValue arg, Continuation * cont, Env * env)
    : Continuation("Cond Apply")
    , arg(arg)
    , cont(cont)
    , env(env)
{
}

CondApplyCont *
CondApplyCont::makeInstance(ScamValue arg, Continuation * cont, Env * env)
{
    return new CondApplyCont(arg, cont, env);
}

void CondApplyCont::mark()
{
    arg->mark();
    cont->mark();
    env->mark();
}

void CondApplyCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        apply(value, makeList(arg), cont, env);
    }
}
