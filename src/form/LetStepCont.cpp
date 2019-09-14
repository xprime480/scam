#include "form/LetStepCont.hpp"

#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "env/Env.hpp"
#include "expr/SequenceOps.hpp"
#include "form/LetEvalWorker.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"

using namespace scam;
using namespace std;

LetStepCont::LetStepCont(ScamValue formals,
                         ScamValue forms,
                         ScamValue evaled,
                         ScamValue args,
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

LetStepCont * LetStepCont::makeInstance(ScamValue formals,
                                        ScamValue forms,
                                        ScamValue evaled,
                                        ScamValue args,
                                        Continuation * cont,
                                        Env * env,
                                        bool rebind)
{
    return new LetStepCont(formals, forms, evaled, args, cont, env, rebind);
}

void LetStepCont::mark()
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

void LetStepCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
    }
    else {
        ScamValue extend = append(evaled, value);
        workQueueHelper<LetEvalWorker>(formals,
                                       extend,
                                       args,
                                       forms,
                                       cont,
                                       env,
                                       rebind);
    }
}
