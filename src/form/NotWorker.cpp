#include "form/NotWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/NotCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

NotWorker::NotWorker(Continuation * cont, Env * env, ScamExpr * args)
    : Worker("Not")
    , args(args)
    , cont(cont)
    , env(env)
{
}

NotWorker *
NotWorker::makeInstance(Continuation * cont, Env * env, ScamExpr * args)
{
    return new NotWorker(cont, env, args);
}

void NotWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void NotWorker::run()
{
    Worker::run();

    if ( ! args->isList() || 1u != args->length() ) {
        ScamExpr * err =
            ExpressionFactory::makeError("Not expects 1 form; got: ",
                                         args->toString());
        cont->run(err);
    }
    else {
        Continuation * newCont = standardMemoryManager.make<NotCont>(cont);
        ScamExpr * test = args->nthcar(0);
        test->eval(newCont, env);
    }
}

