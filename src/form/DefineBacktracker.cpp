#include "form/DefineBacktracker.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueWriter.hpp"

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

    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    Continuation * cont = mm.make<Continuation>("Define Backtrack");
    runParent(cont);
}
