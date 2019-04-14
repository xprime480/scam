#include "form/OrWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/OrCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

OrWorker::OrWorker(Continuation * cont,
                   Env * env,
                   ScamExpr * args,
                   size_t n)
    : Worker("Or")
    , args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

OrWorker * OrWorker::makeInstance(Continuation * cont,
                                  Env * env,
                                  ScamExpr * args,
                                  size_t n)
{
    return new OrWorker(cont, env, args, n);
}

void OrWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void OrWorker::run()
{
    Worker::run();

    if ( ! args->isList() ) {
        ScamExpr * err =
            ExpressionFactory::makeError("Or expects a list of forms; got: ",
                                         args->toString());
        cont->run(err);
        return;
    }

    size_t const len = args->length();
    if ( 0 == len ) {
        ScamExpr * rv = ExpressionFactory::makeBoolean(false);
        cont->run(rv);
    }
    else if ( n == (len - 1) ) {
        args->nthcar(len-1)->eval(cont, env);
    }
    else {
        ScamExpr * test = args->nthcar(n);

        Continuation * newCont =
            standardMemoryManager.make<OrCont>(args, cont, env, n+1);
        test->eval(newCont, env);
    }
}
