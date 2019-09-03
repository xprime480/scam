#include "expr/MapWorker.hpp"

#include "env/Env.hpp"
#include "expr/CarContinuation.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

MapWorker::MapWorker(ScamValue car,
                     ScamValue cdr,
                     Continuation * original,
                     Env * env)
    : Worker("Cons Map")
    , car(car)
    , env(env)
{
    cont = standardMemoryManager.make<CarContinuation>(cdr, original, env);
}

MapWorker *
MapWorker::makeInstance(ScamValue car,
                        ScamValue cdr,
                        Continuation * original,
                        Env * env)
{
    return new MapWorker(car, cdr, original, env);
}

void MapWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        car->mark();
        cont->mark();
        env->mark();
    }
}

void MapWorker::run()
{
    Worker::run();
    eval(car, cont, env);
}
