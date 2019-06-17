#include "expr/MapWorker.hpp"

#include "expr/CarContinuation.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

MapWorker::MapWorker(Continuation * cont,
                     Env * env,
                     ScamValue car,
                     ScamValue cdr,
                     ScamEngine * engine)
    : Worker("Cons Map", engine)
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<CarContinuation>(data, engine);
}

MapWorker * MapWorker::makeInstance(Continuation * cont,
                                    Env * env,
                                    ScamValue car,
                                    ScamValue cdr,
                                    ScamEngine * engine)
{
    return new MapWorker(cont, env, car, cdr, engine);
}

void MapWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        data.mark();
    }
}

void MapWorker::run()
{
    Worker::run();
    eval(data.car, data.cont, data.env, engine);
}
