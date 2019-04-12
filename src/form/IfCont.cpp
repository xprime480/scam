#include "form/IfCont.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

IfCont::IfCont(ScamExpr * args, Continuation * cont, Env * env)
    : Continuation("If")
    , args(args)
    , cont(cont)
    , env(env)
{
}

IfCont * IfCont::makeInstance(ScamExpr * args, Continuation * cont, Env * env)
{
    return new IfCont(args, cont, env);
}

void IfCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void IfCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( expr->truth() ) {
        args->nthcar(1)->eval(cont, env);
    }
    else if ( args->length() > 2 ) {
        args->nthcar(2)->eval(cont, env);
    }
    else {
        cont->run(ExpressionFactory::makeNil());
    }
}
