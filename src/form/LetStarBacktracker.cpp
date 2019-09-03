#include "form/LetStarBacktracker.hpp"

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

LetStarBacktracker::LetStarBacktracker(Env * env,
                                       ScamValue sym,
                                       Backtracker * backtracker)
    : Backtracker("Let*", backtracker)
    , env(env)
    , sym(sym)
{
}

LetStarBacktracker *
LetStarBacktracker::makeInstance(Env * env,
                                 ScamValue sym,
                                 Backtracker * backtracker)
{
    return new LetStarBacktracker(env, sym, backtracker);
}

void LetStarBacktracker::mark()
{
    if ( ! isMarked() ) {
        Backtracker::mark();
        env->mark();
        sym->mark();
    }
}

void LetStarBacktracker::run()
{
    Backtracker::run();
    ScamValue test = env->remove(sym);
    if ( isError(test) ) {
        throw ScamException(writeValue(test));
    }
    MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
    Continuation * cont = mm.make<Continuation>("Assign Backtrack");
    runParent(cont);
}
