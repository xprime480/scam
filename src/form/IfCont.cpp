#include "form/IfCont.hpp"

#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
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

void IfCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( error(expr) ) {
        cont->run(expr);
    }
    else if ( truth(expr) ) {
        ScamValue thenExpr = parser->get(1u);
        eval(thenExpr, cont, env);
    }
    else if ( parser->size() > 2 ) {
        ScamValue elseExpr = parser->get(2u);
        eval(elseExpr, cont, env);
    }
    else {
        cont->run(makeNull());
    }
}
