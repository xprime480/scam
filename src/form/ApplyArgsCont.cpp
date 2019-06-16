#include "form/ApplyArgsCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ApplyArgsCont::ApplyArgsCont(ScamValue op,
                             Continuation * cont,
                             Env * env,
                             ScamEngine * engine)
    : Continuation("apply args", engine)
    , op(op)
    , cont(cont)
    , env(env)
{
}

ApplyArgsCont * ApplyArgsCont::makeInstance(ScamValue op,
                                            Continuation * cont,
                                            Env * env,
                                            ScamEngine * engine)
{
    return new ApplyArgsCont(op, cont, env, engine);
}

void ApplyArgsCont::mark() const
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

    if ( isError(value) ) {
        engine->handleError(value);
    }
    else {
        apply(op, value, cont, env, engine);
    }
}
