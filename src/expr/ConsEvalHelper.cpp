#include "expr/ConsWorker.hpp"

#include "expr/EvalOps.hpp"
#include "expr/ExprEvalCont.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ConsWorker::ConsWorker(Continuation * cont,
                       Env * env,
                       ScamValue car,
                       ScamValue cdr,
                       ScamEngine * engine)

    : Worker("Cons Eval", engine)
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<ExprEvalCont>(data, engine);
}

ConsWorker::ConsWorker(WorkerData const & data, ScamEngine * engine)
    : Worker("Cons Eval Copy", engine)
    , data(data)
{
}

ConsWorker * ConsWorker::makeInstance(Continuation * cont,
                                      Env * env,
                                      ScamValue car,
                                      ScamValue cdr,
                                      ScamEngine * engine)
{
    return new ConsWorker(cont, env, car, cdr, engine);
}

ConsWorker *
ConsWorker::makeInstance(WorkerData const & data, ScamEngine * engine)
{
    return new ConsWorker(data, engine);
}

void ConsWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        data.mark();
    }
}

void ConsWorker::run()
{
    Worker::run();
    eval(data.car, data.cont, data.env, engine);
}
