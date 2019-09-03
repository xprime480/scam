#include "form/DefineBacktracker.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

DefineBacktracker::DefineBacktracker(ScamValue sym,
                                     Env * env,
                                     Backtracker * backtracker)
    : Backtracker("DefineBacktracker", backtracker)
    , sym(sym)
    , env(env)
{
}

DefineBacktracker *
DefineBacktracker::makeInstance(ScamValue sym,
                                Env * env,
                                Backtracker * backtracker)
{
    return new DefineBacktracker(sym, env, backtracker);
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
    ScamValue test = env->remove(sym);
    if ( isError(test) ) {
        throw ScamException(writeValue(test));
    }

    Continuation * cont
        = standardMemoryManager.make<Continuation>("Define Backtrack");
    runParent(cont);
}
