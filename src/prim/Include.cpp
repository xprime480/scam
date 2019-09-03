#include "prim/Include.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "prim/IncludeWorker.hpp"
#include "util/MemoryManager.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyInclude(ScamValue args, Continuation * cont)
{
    static const char * name = "include";
    StringParameter  pStr;
    CountedParameter p0(pStr, 1);
    if ( argsToParms(args, name, p0) ) {
        workQueueHelper<IncludeWorker>(p0.value, cont);
    }
}
