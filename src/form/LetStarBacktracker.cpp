#include "form/LetStarBacktracker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

LetStarBacktracker::LetStarBacktracker(Env * env,
                                       ScamExpr * sym,
                                       Backtracker * backtracker)
    : Backtracker("Let*", backtracker)
    , env(env)
    , sym(sym)
{
}

LetStarBacktracker *
LetStarBacktracker::makeInstance(Env * env,
                                 ScamExpr * sym,
                                 Backtracker * backtracker)
{
    return new LetStarBacktracker(env, sym, backtracker);
}

void LetStarBacktracker::mark() const
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
    env->remove(sym);
    Continuation * cont =
        standardMemoryManager.make<Continuation>("Assign Backtrack");
    runParent(cont);
}
