#include "form/Or.hpp"

#include "WorkQueue.hpp"
#include "form/OrWorker.hpp"

using namespace scam;
using namespace std;

namespace scam
{
    class MemoryManager;
}

Or::Or()
    : SpecialForm("or")
{
}

Or * Or::makeInstance()
{
    static Or instance;
    return &instance;
}

void Or::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    unsigned pos { 0 };
    workQueueHelper<OrWorker>(cont, env, args, pos);
}
