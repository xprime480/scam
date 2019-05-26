#include "expr/MapWorker.hpp"

#include "expr/CarContinuation.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamExpr.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

MapWorker::MapWorker(Continuation * cont,
                     Env * env,
                     ScamValue car,
                     ScamValue cdr)
    : Worker("Cons Map")
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<CarContinuation>(data);
}

MapWorker * MapWorker::makeInstance(Continuation * cont,
                                    Env * env,
                                    ScamValue car,
                                    ScamValue cdr)
{
    return new MapWorker(cont, env, car, cdr);
}

void MapWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        data.mark();
    }
}

void MapWorker::run()
{
    Worker::run();
    eval(data.car, data.cont, data.env);
}
