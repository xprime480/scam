#include "form/AssignBacktracker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AssignBacktracker::AssignBacktracker(ScamValue sym,
                                     ScamValue old,
                                     Env * env,
                                     Backtracker * backtracker,
                                     ScamEngine * engine)
    : Backtracker("DefineBacktracker", backtracker)
    , sym(sym)
    , old(old)
    , env(env)
    , engine(engine)
{
}

AssignBacktracker * AssignBacktracker::makeInstance(ScamValue sym,
                                                    ScamValue old,
                                                    Env * env,
                                                    Backtracker * backtracker,
                                                    ScamEngine * engine)
{
    return new AssignBacktracker(sym, old, env, backtracker, engine);
}

void AssignBacktracker::mark() const
{
    if ( ! isMarked() ) {
        Backtracker::mark();
        sym->mark();
        old->mark();
        env->mark();
    }
}

void AssignBacktracker::run()
{
    Backtracker::run();
    env->assign(sym, old);
    Continuation * cont
        = standardMemoryManager.make<Continuation>("Assign Backtrack", engine);
    runParent(cont);
}
