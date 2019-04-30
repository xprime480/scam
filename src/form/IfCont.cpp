#include "form/IfCont.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

IfCont::IfCont(CountedListParser * parser, Continuation * cont, Env * env)
    : Continuation("If")
    , parser(parser)
    , cont(cont)
    , env(env)
{
}

IfCont * IfCont::makeInstance(CountedListParser * parser,
                              Continuation * cont,
                              Env * env)
{
    return new IfCont(parser, cont, env);
}

void IfCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void IfCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( expr->truth() ) {
        ExprHandle thenExpr = parser->get(1u);
        thenExpr->eval(cont, env);
    }
    else if ( parser->size() > 2 ) {
        ExprHandle elseExpr = parser->get(2u);
        elseExpr->eval(cont, env);
    }
    else {
        cont->run(ExpressionFactory::makeNil());
    }
}
