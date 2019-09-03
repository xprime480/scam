#include "expr/MapCdr.hpp"

#include "env/Env.hpp"
#include "expr/CdrContinuation.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

MapCdr::MapCdr(ScamValue car, ScamValue cdr, Continuation * original, Env * env)
    : Worker("Cons Map Cdr")
    , cdr(cdr)
    , env(env)
{
    cont = standardMemoryManager.make<CdrContinuation>(car, original);
}

MapCdr *
MapCdr::makeInstance(ScamValue car,
                     ScamValue cdr,
                     Continuation * original,
                     Env * env)
{
    return new MapCdr(car, cdr, original, env);
}

void MapCdr::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        cdr->mark();
        cont->mark();
        env->mark();
    }
}

void MapCdr::run()
{
    Worker::run();
    mapEval(cdr, cont, env);
}
