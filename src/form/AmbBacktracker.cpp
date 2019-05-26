#include "form/AmbBacktracker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AmbBacktracker::AmbBacktracker(ScamValue args,
                               Continuation * cont,
                               Env * env,
                               ScamEngine * engine,
                               Backtracker * parent)
    : Backtracker("AmbBacktracker", parent)
    , args(args)
    , cont(cont)
    , env(env)
    , engine(engine)
{
}

AmbBacktracker * AmbBacktracker::makeInstance(ScamValue args,
                                              Continuation * cont,
                                              Env * env,
                                              ScamEngine * engine,
                                              Backtracker * parent)
{
    return new AmbBacktracker(args, cont, env, engine, parent);
}

void AmbBacktracker::mark() const
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
        ScamValue head = nthcar(args, 0);
        ScamValue tail = nthcdr(args, 0);

        (void) engine->getBacktracker();
        Backtracker * newBt =
            standardMemoryManager.make<AmbBacktracker>(tail,
                                                       cont,
                                                       env,
                                                       engine,
                                                       getParent());
        engine->setBacktracker(newBt);

        eval(head, cont, env);
    }
}
