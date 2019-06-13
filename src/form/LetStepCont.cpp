#include "form/LetStepCont.hpp"

#include "Env.hpp"
#include "WorkQueue.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "form/LetEvalWorker.hpp"

using namespace scam;
using namespace std;

LetStepCont::LetStepCont(ScamValue formals,
                         ScamValue forms,
                         ScamValue evaled,
                         ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine,
                         bool rebind)
    : Continuation("LetStepCont", engine)
    , formals(formals)
    , forms(forms)
    , evaled(evaled)
    , args(args)
    , cont(cont)
    , env(env)
    , rebind(rebind)
{
}

LetStepCont * LetStepCont::makeInstance(ScamValue formals,
                                        ScamValue forms,
                                        ScamValue evaled,
                                        ScamValue args,
                                        Continuation * cont,
                                        Env * env,
                                        ScamEngine * engine,
                                        bool rebind)
{
    return new LetStepCont(formals,
                           forms,
                           evaled,
                           args,
                           cont,
                           env,
                           engine,
                           rebind);
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

void LetStepCont::run(ScamValue expr)
{
    if ( error(expr) ) {
        cont->run(expr);
    }
    else {
        ScamValue extend = append(evaled, expr);
        workQueueHelper<LetEvalWorker>(formals,
                                       extend,
                                       args,
                                       forms,
                                       cont,
                                       env,
                                       engine,
                                       rebind);
    }
}
