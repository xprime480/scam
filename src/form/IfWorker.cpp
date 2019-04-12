#include "form/IfWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/IFCont.hpp"
#include "util/MemoryManager.hpp"

#include <sstream>

using namespace scam;
using namespace std;

IfWorker::IfWorker(Continuation * cont, Env * env, ScamExpr * args)
    : Worker("If")
    , args(args)
    , cont(cont)
    , env(env)
{
}

IfWorker *
IfWorker::makeInstance(Continuation * cont, Env * env, ScamExpr * args)
{
    return new IfWorker(cont, env, args);
}

void IfWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        cont->mark();
        env->mark();
        args->mark();
    }
}

void IfWorker::run()
{
    Worker::run();

    if ( ! args->isList() || args->length() < 2 || args->length() > 3 ) {
        stringstream s;
        s << "If expects 2 or 3 forms; got: " << args->toString();
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        Continuation * newCont =
            standardMemoryManager.make<IfCont>(args, cont, env);
        ScamExpr * test = args->nthcar(0);

        test->eval(newCont, env);
    }
}
