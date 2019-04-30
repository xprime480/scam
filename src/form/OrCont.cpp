#include "form/OrCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "form/OrWorker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

OrCont::OrCont(ExprHandle args, Continuation * cont, Env * env, size_t n)
    : Continuation("Or")
    , args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

OrCont *
OrCont::makeInstance(ExprHandle args, Continuation * cont, Env * env, size_t n)
{
  return new OrCont(args, cont, env, n);
}

void OrCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        args->mark();
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
        workQueueHelper<OrWorker>(cont, env, args, n);
    }
}
