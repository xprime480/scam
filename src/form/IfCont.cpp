#include "form/IfCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

IfCont::IfCont(ScamValue args, Continuation * cont, Env * env)
    : Continuation("If")
    , args(args)
    , cont(cont)
    , env(env)
{
}

IfCont * IfCont::makeInstance(ScamValue args, Continuation * cont, Env * env)
{
    return new IfCont(args, cont, env);
}

void IfCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void IfCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else if ( truth(value) ) {
        ScamValue thenExpr = nthcar(args, 1);
        eval(thenExpr, cont, env);
    }
    else if ( length(args) > 2 ) {
        ScamValue elseExpr = nthcar(args, 2);
        eval(elseExpr, cont, env);
    }
    else {
        cont->handleValue(makeNull());
    }
}
