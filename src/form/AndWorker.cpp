#include "form/AndWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/AndCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AndWorker::AndWorker(Continuation * cont,
                     Env * env,
                     ExprHandle args,
                     size_t n)
    : Worker("And")
    , args(args)
    , cont(cont)
    , env(env)
    , n(n)
{
}

AndWorker * AndWorker::makeInstance(Continuation * cont,
                                    Env * env,
                                    ExprHandle args,
                                    size_t n)
{
    return new AndWorker(cont, env, args, n);
}

void AndWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void AndWorker::run()
{
    Worker::run();

    if ( ! args->isList() ) {
        ExprHandle err =
            ExpressionFactory::makeError("And expects a list of forms; ",
                                         "got: ",
                                         args->toString());
        cont->run(err);
        return;
    }

    size_t const len = args->length();
    if ( 0 == len ) {
        cont->run(ExpressionFactory::makeBoolean(true));
    }
    else if ( n == (len - 1) ) {
        args->nthcar(len-1)->eval(cont, env);
    }
    else {
        ExprHandle test = args->nthcar(n);
        ExprHandle a = args;
        Continuation * newCont =
            standardMemoryManager.make<AndCont>(a, cont, env, n+1);
        test->eval(newCont, env);
    }
}
