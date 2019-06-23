#include "prim/Spawn.hpp"

#include "WorkQueue.hpp"
#include "prim/SpawnWorker.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applySpawn(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    static const char * name = "spawn";
    if ( argsToParms(args, engine, name) ) {
        workQueueHelper<SpawnWorker>(cont, engine, true);
        workQueueHelper<SpawnWorker>(cont, engine, false);
    }
}
