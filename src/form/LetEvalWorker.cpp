#include "form/LetEvalWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "form/LetStepCont.hpp"
#include "form/LetCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

LetEvalWorker::LetEvalWorker(ScamExpr * formals,
                             ScamExpr * evaled,
                             ScamExpr * args,
                             ScamExpr * forms,
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

LetEvalWorker * LetEvalWorker::makeInstance(ScamExpr * formals,
                                            ScamExpr * evaled,
                                            ScamExpr * args,
                                            ScamExpr * forms,
                                            Continuation * cont,
                                            Env * env,
                                            bool rebind)
{
    return new LetEvalWorker(formals,
                             evaled,
                             args,
                             forms,
                             cont,
                             env,
                             rebind);
}

void LetEvalWorker::mark() const
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

    if ( args->length() > 0 ) {
        ScamExpr * car = args->nthcar(0);
        ScamExpr * cdr = args->nthcdr(0);

        Continuation * ch
            = standardMemoryManager.make<LetStepCont>(formals,
                                                      forms,
                                                      evaled,
                                                      cdr,
                                                      cont,
                                                      env,
                                                      rebind);
        car->eval(ch, env);
    }
    else {
        Continuation * ch
            = standardMemoryManager.make<LetCont>(formals,
                                                  forms,
                                                  cont,
                                                  env,
                                                  rebind);
        ch->run(evaled);
    }
}
