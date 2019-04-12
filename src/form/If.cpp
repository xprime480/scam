#include "form/If.hpp"

#include "WorkQueue.hpp"
#include "form/IfWorker.hpp"

using namespace scam;
using namespace std;

namespace scam
{
    class MemoryManager;
}

If::If()
    : SpecialForm("if")
{
}

If * If::makeInstance()
{
    static If instance;
    return &instance;
}

void If::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    workQueueHelper<IfWorker>(cont, env, args);
}
