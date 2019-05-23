#include "form/AndCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "form/AndWorker.hpp"
#include "input/ListParser.hpp"

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

void AndCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( TypePredicates::error(expr) ) {
        cont->run(expr);
    }
    else if ( ! TypePredicates::truth(expr) ) {
        cont->run(expr);
    }
    else {
        workQueueHelper<AndWorker>(cont, env, parser, n);
    }
}
