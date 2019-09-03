#include "form/AssignBacktracker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AssignBacktracker::AssignBacktracker(ScamValue sym,
                                     ScamValue old,
                                     Env * env,
                                     Backtracker * backtracker)
    : Backtracker("DefineBacktracker", backtracker)
    , sym(sym)
    , old(old)
    , env(env)
{
}

AssignBacktracker * AssignBacktracker::makeInstance(ScamValue sym,
                                                    ScamValue old,
                                                    Env * env,
                                                    Backtracker * backtracker)
{
    return new AssignBacktracker(sym, old, env, backtracker);
}

void AssignBacktracker::mark()
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

    ScamValue test = env->assign(sym, old);
    if ( isError(test) ) {
        throw ScamException(writeValue(test));
    }
    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    Continuation * cont = mm.make<Continuation>("Assign Backtrack");
    runParent(cont);
}
