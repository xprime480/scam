#include "expr/ConsWorker.hpp"

#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ExprEvalCont.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ConsWorker::ConsWorker(ScamValue car,
                       ScamValue cdr,
                       Continuation * original,
                       Env * env,
                       ScamEngine * engine)

    : Worker("Cons Eval", engine)
    , car(car)
    , env(env)
{
    cont =
        standardMemoryManager.make<ExprEvalCont>(cdr, original, env, engine);
}

ConsWorker * ConsWorker::makeInstance(ScamValue car,
                                      ScamValue cdr,
                                      Continuation * original,
                                      Env * env,
                                      ScamEngine * engine)
{
    return new ConsWorker(car, cdr, original, env, engine);
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
    eval(car, cont, env, engine);
}
