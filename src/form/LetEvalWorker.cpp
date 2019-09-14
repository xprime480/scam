#include "form/LetEvalWorker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "form/LetCont.hpp"
#include "form/LetStepCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"

using namespace scam;
using namespace std;

LetEvalWorker::LetEvalWorker(ScamValue formals,
                             ScamValue evaled,
                             ScamValue args,
                             ScamValue forms,
                             Continuation * cont,
                             Env * env,
                             bool rebind)
    : Worker("LetEvalWorker")
    , formals(formals)
    , evaled(evaled)
    , args(args)
    , forms(forms)
    , cont(cont)
    , env(env)
    , rebind(rebind)
{
}

LetEvalWorker * LetEvalWorker::makeInstance(ScamValue formals,
                                            ScamValue evaled,
                                            ScamValue args,
                                            ScamValue forms,
                                            Continuation * cont,
                                            Env * env,
                                            bool rebind)
{
    return new LetEvalWorker(formals, evaled, args, forms, cont, env, rebind);
}

void LetEvalWorker::mark()
{
    if ( ! isMarked() ) {
        Worker::mark();
        formals->mark();
        evaled->mark();
        args->mark();
        forms->mark();
        cont->mark();
        env->mark();
    }
}

void LetEvalWorker::run()
{
    Worker::run();

    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();

    if ( length(args) > 0 ) {
        ScamValue car = nthcar(args, 0);
        ScamValue cdr = nthcdr(args, 0);

        Continuation * ch
            = mm.make<LetStepCont>(formals,
                                   forms,
                                   evaled,
                                   cdr,
                                   cont,
                                   env,
                                   rebind);
        eval(car, ch, env);
    }
    else {
        Continuation * ch = mm.make<LetCont>(formals, forms, cont, env, rebind);
        ch->handleValue(evaled);
    }
}
