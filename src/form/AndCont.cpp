#include "form/AndCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "input/ListParser.hpp"
#include "form/AndWorker.hpp"

using namespace scam;
using namespace std;

AndCont::AndCont(ListParser * parser, Continuation * cont, Env * env, size_t n)
    : Continuation("And")
    , parser(parser)
    , cont(cont)
    , env(env)
    , n(n)
{
}

AndCont * AndCont::makeInstance(ListParser * parser,
                                Continuation * cont,
                                Env * env,
                                size_t n)
{
    return new AndCont(parser, cont, env, n);
}

void AndCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void AndCont::run(ExprHandle expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( ! expr->truth() ) {
        cont->run(expr);
    }
    else {
        workQueueHelper<AndWorker>(cont, env, parser, n);
    }
}
