#include "expr/MapCdr.hpp"

#include "expr/CdrContinuation.hpp"
#include "expr/ScamExpr.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

MapCdr::MapCdr(ScamExpr * car,
               ScamExpr * cdr,
               Continuation * cont,
               Env * env)
    : Worker("Cons Map Cdr")
    , data(car, cdr, cont, env)
{
    data.cont = standardMemoryManager.make<CdrContinuation>(data);
}

MapCdr * MapCdr::makeInstance(ScamExpr * car,
                              ScamExpr * cdr,
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
    data.cdr->mapEval(data.cont, data.env);
}

