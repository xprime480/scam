#include "form/Let.hpp"

#include "WorkQueue.hpp"
#include "form/LetWorker.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

Let::Let()
    : SpecialForm("let")
{
}

Let * Let::makeInstance()
{
    static Let instance;
    return &instance;
}

void Let::apply(ExprHandle args, Continuation * cont, Env * env)
{
    workQueueHelper<LetWorker>(args, cont, env, false);
}
