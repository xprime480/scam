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
    workQueueHelper<ClassWorker>(this, args, cont, env);
}

bool ScamClass::isProcedure() const
{
    return true;
}

bool ScamClass::isClass() const
{
    return true;
}
