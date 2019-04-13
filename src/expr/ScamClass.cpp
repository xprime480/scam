#include "expr/ScamClass.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ClassWorker.hpp"

using namespace scam;
using namespace std;

ScamClass::ScamClass(ScamExpr * base,
                     ScamExpr * vars,
                     ScamExpr * funs,
                     Env * capture)
    : base(base)
    , vars(vars)
    , funs(funs)
    , capture(capture)
{
}

ScamClass * ScamClass::makeInstance(ScamExpr * base,
                                    ScamExpr * vars,
                                    ScamExpr * funs,
                                    Env * capture)
{
    return new ScamClass(base, vars, funs, capture);
}

void ScamClass::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        base->mark();
        vars->mark();
        funs->mark();
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

void ScamClass::apply(ScamExpr * args, Continuation * cont, Env * env)
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
