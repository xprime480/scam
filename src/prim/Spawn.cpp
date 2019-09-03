#include "prim/Spawn.hpp"

#include "WorkQueue.hpp"
#include "prim/SpawnWorker.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applySpawn(ScamValue args, Continuation * cont)
{
    static const char * name = "spawn";
    if ( argsToParms(args, name) ) {
        workQueueHelper<SpawnWorker>(cont, true);
        workQueueHelper<SpawnWorker>(cont, false);
    }
}
