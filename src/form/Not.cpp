#include "form/Not.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/NotWorker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace scam
{
    class MemoryManager;
}

Not::Not()
    : SpecialForm("not")
{
}

Not * Not::makeInstance()
{
    static Not instance;
    return &instance;
}

void Not::apply(ExprHandle args, Continuation * cont, Env * env)
{
    workQueueHelper<NotWorker>(cont, env, args);
}
