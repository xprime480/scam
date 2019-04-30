#include "form/LetStepCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ScamListAdapter.hpp"
#include "form/LetEvalWorker.hpp"

using namespace scam;
using namespace std;

LetStepCont::LetStepCont(ExprHandle formals,
                         ExprHandle forms,
                         ExprHandle evaled,
                         ExprHandle args,
                         Continuation * cont,
                         Env * env,
                         bool rebind)
    : Continuation("LetStepCont")
    , formals(formals)
    , forms(forms)
    , evaled(evaled)
    , args(args)
    , cont(cont)
    , env(env)
    , rebind(rebind)
{
}

LetStepCont * LetStepCont::makeInstance(ExprHandle formals,
                                        ExprHandle forms,
                                        ExprHandle evaled,
                                        ExprHandle args,
                                        Continuation * cont,
                                        Env * env,
                                        bool rebind)
{
    return new LetStepCont(formals, forms, evaled, args, cont, env, rebind);
}

void LetStepCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        formals->mark();
        forms->mark();
        evaled->mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void LetStepCont::run(ExprHandle expr)
{
    if ( expr->error() ) {
        cont->run(expr);
    }
    else {
        ScamListAdapter a(evaled);
        ExprHandle extend = a.append(expr);
        workQueueHelper<LetEvalWorker>(formals,
                                       extend,
                                       args,
                                       forms,
                                       cont,
                                       env,
                                       rebind);
    }
}
