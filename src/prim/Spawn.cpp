
#include "prim/Spawn.hpp"

#include "WorkQueue.hpp"
#include "prim/SpawnWorker.hpp"

using namespace scam;
using namespace std;

Spawn::Spawn()
    : Primitive("spawn")
{
}

Spawn * Spawn::makeInstance()
{
    return new Spawn();
}

void Spawn::applyArgs(ExprHandle args, Continuation * cont)
{
    workQueueHelper<SpawnWorker>(cont, true);
    workQueueHelper<SpawnWorker>(cont, false);
}
