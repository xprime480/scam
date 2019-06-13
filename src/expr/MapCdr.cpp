#include "expr/MapCdr.hpp"

#include "expr/CdrContinuation.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

MapCdr::MapCdr(ScamValue car,
               ScamValue cdr,
               Continuation * cont,
               Env * env,
               ScamEngine * engine)
    : Worker("Cons Map Cdr", engine)
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<CdrContinuation>(data, engine);
}

MapCdr * MapCdr::makeInstance(ScamValue car,
                              ScamValue cdr,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine)
{
    return new MapCdr(car, cdr, cont, env, engine);
}

void MapCdr::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        data.mark();
    }
}

void MapCdr::run()
{
    Worker::run();
    mapEval(data.cdr, data.cont, data.env, engine);
}
