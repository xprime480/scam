#include "form/LetStarBacktracker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

LetStarBacktracker::LetStarBacktracker(Env * env,
                                       ScamValue sym,
                                       Backtracker * backtracker,
                                       ScamEngine * engine)
    : Backtracker("Let*", backtracker)
    , env(env)
    , sym(sym)
    , engine(engine)
{
}

LetStarBacktracker *
LetStarBacktracker::makeInstance(Env * env,
                                 ScamValue sym,
                                 Backtracker * backtracker,
                                 ScamEngine * engine)
{
    return new LetStarBacktracker(env, sym, backtracker, engine);
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
    env->remove(sym);
    Continuation * cont =
        standardMemoryManager.make<Continuation>("Assign Backtrack", engine);
    runParent(cont);
}
