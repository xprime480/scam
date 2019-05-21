#include "expr/ScamClass.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassWorker.hpp"
#include "input/ClassDefParser.hpp"

using namespace scam;
using namespace std;

ScamClass::ScamClass(ClassDefParser * def, Env * capture)
    : def(def)
    , capture(capture)
{
    data.type = ScamData::Class;
}

ScamClass * ScamClass::makeInstance(ClassDefParser * def, Env * capture)
{
    return new ScamClass(def, capture);
}

void ScamClass::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        def->mark();
        capture->mark();
    }
}

string ScamClass::toString() const
{
    return "class";
}

bool ScamClass::hasApply() const
{
    return true;
}

void ScamClass::apply(ExprHandle args, Continuation * cont, Env * env)
{
    /** It is not meaningful to do argument validation here as the
     ** correct args are not apparant until the instance init method
     ** is found.
     **/
    workQueueHelper<ClassWorker>(this, args, cont, env);
}
