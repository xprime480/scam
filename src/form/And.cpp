#include "form/And.hpp"

#include "WorkQueue.hpp"
#include "form/AndWorker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

And::And()
    : SpecialForm("and")
{
}

And * And::makeInstance()
{
    static And instance;
    return &instance;
}

void And::apply(ExprHandle args, Continuation * cont, Env * env)
{
    unsigned pos { 0 };
    workQueueHelper<AndWorker>(cont, env, args, pos);
}
