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

namespace
{
    extern void apply_impl(ScamExpr * args, Continuation * cont, Env * env);
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

void Not::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    workQueueHelper<NotWorker>(cont, env, args);
}
