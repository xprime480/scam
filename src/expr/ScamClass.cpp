#include "expr/ScamClass.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassWorker.hpp"
#include "input/ClassDefParser.hpp"

using namespace scam;
using namespace std;

ScamClass::ScamClass(ClassDefParser * def, Env * capture)
    : ScamExpr(ScamData::Class)
{
    CLASSDEF(this) = def;
    CLASSENV(this) = capture;
}

ScamClass * ScamClass::makeInstance(ClassDefParser * def, Env * capture)
{
    return new ScamClass(def, capture);
}

void ScamClass::apply(ExprHandle args, Continuation * cont, Env * env)
{
    /** It is not meaningful to do argument validation here as the
     ** correct args are not apparant until the instance init method
     ** is found.
     **/
    workQueueHelper<ClassWorker>(this, args, cont, env);
}
