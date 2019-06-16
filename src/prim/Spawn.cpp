#include "prim/Spawn.hpp"

#include "WorkQueue.hpp"
#include "expr/TypePredicates.hpp"
#include "prim/SpawnWorker.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "spawn";

void scam::applySpawn(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    if ( ! isNull(args) ) {
        failedArgParseMessage(myName, "()", args, cont, engine);
    }
    else {
        workQueueHelper<SpawnWorker>(cont, engine, true);
        workQueueHelper<SpawnWorker>(cont, engine, false);
    }
}

