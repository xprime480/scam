#include "form/AndCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "form/AndWorker.hpp"

using namespace scam;
using namespace std;

AndCont::AndCont(ScamExpr * args, Continuation * cont, Env * env, size_t n)
    : Continuation("And")
    , args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

AndCont *
AndCont::makeInstance(ScamExpr * args, Continuation * cont, Env * env, size_t n)
{
    return new AndCont(args, cont, env, n);
}

void AndCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void AndCont::run(ScamExpr * expr)
{
    Continuation::run(expr);

    if ( expr->error() ) {
        cont->run(expr);
    }
    else if ( ! expr->truth() ) {
        cont->run(expr);
    }
    else {
        workQueueHelper<AndWorker>(cont, env, args, n);
    }
}
