#include "expr/ConsWorker.hpp"

#include "expr/ExprEvalCont.hpp"
#include "expr/ScamExpr.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ConsWorker::ConsWorker(Continuation * cont,
                       Env * env,
                       ScamExpr * car,
                       ScamExpr * cdr)

    : Worker("Cons Eval")
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<ExprEvalCont>(data);
}

ConsWorker::ConsWorker(WorkerData const & data)
    : Worker("Cons Eval Copy")
    , data(data)
{
}

ConsWorker * ConsWorker::makeInstance(Continuation * cont,
                                      Env * env,
                                      ScamExpr * car,
                                      ScamExpr * cdr)

{
    return new ConsWorker(cont, env, car, cdr);
}

ConsWorker * ConsWorker::makeInstance(WorkerData const & data)
{
    return new ConsWorker(data);
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
    data.car->eval(data.cont, data.env);
}
