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
               Env * env)
    : Worker("Cons Map Cdr")
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<CdrContinuation>(data);
}

MapCdr * MapCdr::makeInstance(ScamValue car,
                              ScamValue cdr,
                              Continuation * cont,
                              Env * env)
{
    return new MapCdr(car, cdr, cont, env);
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
    mapEval(data.cdr, data.cont, data.env);
}

