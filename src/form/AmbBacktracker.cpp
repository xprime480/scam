#include "form/AmbBacktracker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AmbBacktracker::AmbBacktracker(ScamValue args,
                               Continuation * cont,
                               Env * env,
                               Backtracker * parent)
    : Backtracker("AmbBacktracker", parent)
    , args(args)
    , cont(cont)
    , env(env)
{
}

AmbBacktracker * AmbBacktracker::makeInstance(ScamValue args,
                                              Continuation * cont,
                                              Env * env,
                                              Backtracker * parent)
{
    return new AmbBacktracker(args, cont, env, parent);
}

void AmbBacktracker::mark()
{
    if ( ! isMarked() ) {
        Backtracker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void AmbBacktracker::run()
{
    Backtracker::run();

    if ( 0 == length(args) ) {
        runParent(cont);
    }
    else {
        ScamEngine & engine = ScamEngine::getEngine();

        ScamValue head = nthcar(args, 0);
        ScamValue tail = nthcdr(args, 0);

        (void) engine.getBacktracker();
        MemoryManager & mm = engine.getMemoryManager();
        Backtracker * newBt =
            mm.make<AmbBacktracker>(tail, cont, env, getParent());
        engine.setBacktracker(newBt);

        eval(head, cont, env);
    }
}
