#include "expr/ConsWorker.hpp"

#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ExprEvalCont.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ConsWorker::ConsWorker(ScamValue car,
                       ScamValue cdr,
                       Continuation * original,
                       Env * env)

    : Worker("Cons Eval")
    , car(car)
    , env(env)
{
    cont = standardMemoryManager.make<ExprEvalCont>(cdr, original, env);
}

ConsWorker * ConsWorker::makeInstance(ScamValue car,
                                      ScamValue cdr,
                                      Continuation * original,
                                      Env * env)
{
    return new ConsWorker(car, cdr, original, env);
}

void ConsWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        car->mark();
        cont->mark();
        env->mark();
    }
}

void ConsWorker::run()
{
    Worker::run();
    eval(car, cont, env);
}
