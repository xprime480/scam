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

IfCont::IfCont(ScamValue args,
               Continuation * cont,
               Env * env,
               ScamEngine * engine)
    : Continuation("If", engine)
    , args(args)
    , cont(cont)
    , env(env)
{
}

IfCont * IfCont::makeInstance(ScamValue args,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine)
{
    return new IfCont(args, cont, env, engine);
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
        engine->handleError(value);
    }
    else if ( truth(value) ) {
        ScamValue thenExpr = nthcar(args, 1);
        eval(thenExpr, cont, env, engine);
    }
    else if ( length(args) > 2 ) {
        ScamValue elseExpr = nthcar(args, 2);
        eval(elseExpr, cont, env, engine);
    }
    else {
        cont->handleValue(makeNull());
    }
}
