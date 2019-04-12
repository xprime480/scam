#include "form/EnvHelperWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

EnvHelperWorker::EnvHelperWorker(ScamExpr * args,
                                 Continuation * cont,
                                 Env * env,
                                 char const * name)
    : Worker(name)
    , cont(cont)
    , env(env)
    , args(args)
{
}

void EnvHelperWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        cont->mark();
        env->mark();
        args->mark();
    }
}

void EnvHelperWorker::run()
{
    Worker::run();

    ScamExpr * sym = args->getCar();
    Continuation * c = getCont(sym);
    if ( args->length() > 1 ) {
        ScamExpr * expr = args->nthcar(1);
        expr->eval(c, env);
    }
    else {
        ScamExpr * expr = ExpressionFactory::makeNil();
        c->run(expr);
    }
}
