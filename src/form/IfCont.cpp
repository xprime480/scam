#include "form/IfCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

IfCont::IfCont(CountedListParser * parser,
               Continuation * cont,
               Env * env,
               ScamEngine * engine)
    : Continuation("If", engine)
    , parser(parser)
    , cont(cont)
    , env(env)
{
}

IfCont * IfCont::makeInstance(CountedListParser * parser,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine)
{
    return new IfCont(parser, cont, env, engine);
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

void IfCont::handleValue(ScamValue expr)
{
    Continuation::handleValue(expr);

    if ( isError(expr) ) {
        engine->handleError(expr);
    }
    else if ( truth(expr) ) {
        ScamValue thenExpr = parser->get(1u);
        eval(thenExpr, cont, env, engine);
    }
    else if ( parser->size() > 2 ) {
        ScamValue elseExpr = parser->get(2u);
        eval(elseExpr, cont, env, engine);
    }
    else {
        cont->handleValue(makeNull());
    }
}
