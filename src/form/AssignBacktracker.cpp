#include "form/AssignBacktracker.hpp"

#include "Continuation.hpp"
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
    Continuation * cont
        = standardMemoryManager.make<Continuation>("Assign Backtrack", engine);
    runParent(cont);
}
