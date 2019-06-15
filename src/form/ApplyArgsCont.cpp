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

void ApplyArgsCont::handleValue(ScamValue expr)
{
    Continuation::handleValue(expr);

    if ( isError(expr) ) {
        engine->handleError(expr);
    }
    else {
        apply(op, expr, cont, env, engine);
    }
}
