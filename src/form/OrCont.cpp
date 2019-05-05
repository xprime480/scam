#include "form/OrCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "form/OrWorker.hpp"
#include "input/ListParser.hpp"

using namespace scam;
using namespace std;

OrCont::OrCont(ListParser * parser, Continuation * cont, Env * env, size_t n)
    : Continuation("Or")
    , parser(parser)
    , cont(cont)
    , env(env)
    , n(n)
{
}

OrCont * OrCont::makeInstance(ListParser * parser,
                              Continuation * cont,
                              Env * env,
                              size_t n)
{
  return new OrCont(parser, cont, env, n);
}

void OrCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void OrCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( expr->truth() ) {
        cont->run(expr);
    }
    else {
        workQueueHelper<OrWorker>(cont, env, parser, n);
    }
}
