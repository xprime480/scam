#include "expr/MapCdr.hpp"

#include "env/Env.hpp"
#include "expr/CdrContinuation.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

MapCdr::MapCdr(ScamValue car,
               ScamValue cdr,
               Continuation * original,
               Env * env,
               ScamEngine * engine)
    : Worker("Cons Map Cdr", engine)
    , cdr(cdr)
    , env(env)
{
    cont = standardMemoryManager.make<CdrContinuation>(car, original, engine);
}

MapCdr * MapCdr::makeInstance(ScamValue car,
                              ScamValue cdr,
                              Continuation * original,
                              Env * env,
                              ScamEngine * engine)
{
    return new MapCdr(car, cdr, original, env, engine);
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
    mapEval(cdr, cont, env, engine);
}
