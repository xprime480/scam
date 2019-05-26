#include "form/AssignBacktracker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamSymbol.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AssignBacktracker::AssignBacktracker(ScamEnvKeyType sym,
                                     ScamValue old,
                                     Env * env,
                                     Backtracker * backtracker)
    : Backtracker("DefineBacktracker", backtracker)
    , sym(sym)
    , old(old)
    , env(env)
{
}

AssignBacktracker * AssignBacktracker::makeInstance(ScamEnvKeyType sym,
                                                    ScamValue old,
                                                    Env * env,
                                                    Backtracker * backtracker)
{
    return new AssignBacktracker(sym, old, env, backtracker);
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
        = standardMemoryManager.make<Continuation>("Assign Backtrack");
    runParent(cont);
}
