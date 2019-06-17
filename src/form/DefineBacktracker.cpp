#include "form/DefineBacktracker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

DefineBacktracker::DefineBacktracker(ScamValue sym,
                                     Env * env,
                                     Backtracker * backtracker,
                                     ScamEngine * engine)
    : Backtracker("DefineBacktracker", backtracker)
    , sym(sym)
    , env(env)
    , engine(engine)
{
}

DefineBacktracker *
DefineBacktracker::makeInstance(ScamValue sym,
                                Env * env,
                                Backtracker * backtracker,
                                ScamEngine * engine)
{
    return new DefineBacktracker(sym, env, backtracker, engine);
}

void DefineBacktracker::mark()
{
    if ( ! isMarked() ) {
        Backtracker::mark();
        sym->mark();
        env->mark();
    }
}

void DefineBacktracker::run()
{
    Backtracker::run();
    env->remove(sym);
    Continuation * cont
        = standardMemoryManager.make<Continuation>("Define Backtrack", engine);
    runParent(cont);
}
