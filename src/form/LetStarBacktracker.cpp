#include "form/LetStarBacktracker.hpp"

#include "Continuation.hpp"
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
    ScamValue test = env->remove(sym);
    if ( isError(test) ) {
        throw ScamException(writeValue(test));
    }
    Continuation * cont =
        standardMemoryManager.make<Continuation>("Assign Backtrack", engine);
    runParent(cont);
}
