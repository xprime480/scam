#include "form/AmbBacktracker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamExpr.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AmbBacktracker::AmbBacktracker(ScamExpr * args,
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

AmbBacktracker * AmbBacktracker::makeInstance(ScamExpr * args,
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

    if ( 0 == args->length() ) {
        runParent(cont);
    }
    else {
        ScamExpr * head = args->nthcar(0);
        ScamExpr * tail = args->nthcdr(0);

        (void) engine->getBacktracker();
        Backtracker * newBt =
            standardMemoryManager.make<AmbBacktracker>(tail,
                                                       cont,
                                                       env,
                                                       engine,
                                                       getParent());
        engine->setBacktracker(newBt);

        head->eval(cont, env);
    }
}
