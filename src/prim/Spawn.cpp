
#include "prim/Spawn.hpp"

#include "WorkQueue.hpp"
#include "prim/SpawnWorker.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "spawn";

Spawn::Spawn()
    : Primitive(myName)
{
}

Spawn * Spawn::makeInstance()
{
    return new Spawn();
}

void Spawn::applyArgs(ExprHandle args, Continuation * cont)
{
    if ( ! args->isNil() ) {
        failedArgParseMessage(myName, "()", args, cont);
    }
    else {
        workQueueHelper<SpawnWorker>(cont, true);
        workQueueHelper<SpawnWorker>(cont, false);
    }
}
