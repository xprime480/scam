#include "form/LetRec.hpp"

#include "WorkQueue.hpp"
#include "form/LetWorker.hpp"

using namespace scam;
using namespace std;

LetRec::LetRec()
    : SpecialForm("letrec")
{
}

LetRec * LetRec::makeInstance()
{
    static LetRec instance;
    return &instance;
}

void LetRec::apply(ExprHandle args, Continuation * cont, Env * env)
{
    workQueueHelper<LetWorker>(args, cont, env, true);
}
