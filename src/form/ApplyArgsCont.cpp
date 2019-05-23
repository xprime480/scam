#include "form/ApplyArgsCont.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ApplyArgsCont::ApplyArgsCont(ScamValue op, Continuation * cont, Env * env)
    : Continuation("apply args")
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

void ApplyArgsCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        op->mark();
        cont->mark();
        env->mark();
    }
}

void ApplyArgsCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
    }
    else {
        op->apply(expr, cont, env);
    }
}
