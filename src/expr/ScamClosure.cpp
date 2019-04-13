#include "expr/ScamClosure.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ClosureWorker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamClosure::ScamClosure(ScamExpr *formals,
                         ScamExpr * forms,
                         Env * env,
                         bool macrolike)
    : formals(formals)
    , forms(forms)
    , env(env)
    , macrolike(macrolike)
{
}

ScamClosure * ScamClosure::makeInstance(ScamExpr *formals,
                                        ScamExpr * forms,
                                        Env * env,
                                        bool macrolike)
{
    return new ScamClosure(formals, forms, env, macrolike);
}

void ScamClosure::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        formals->mark();
        forms->mark();
        env->mark();
    }
}

string ScamClosure::toString() const
{
    stringstream s;
    s << "(";
    if ( macrolike ) {
        s << "macro ";
    }
    else {
        s << "lambda ";
    }
    s << formals->toString() << " " << forms->toString() << ")";
    return s.str();
}

bool ScamClosure::hasApply() const
{
    return true;
}

void ScamClosure::apply(ScamExpr * args, Continuation * cont, Env * env)
{
        workQueueHelper<ClosureWorker>(formals,
                                       forms,
                                       this->env,
                                       cont,
                                       args,
                                       env,
                                       macrolike);
}

bool ScamClosure::isProcedure() const
{
    return true;
}

ScamExpr * ScamClosure::withEnvUpdate(Env * updated) const
{
    return ExpressionFactory::makeClosure(formals, forms, updated);
}
