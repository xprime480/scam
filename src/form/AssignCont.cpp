 #include "form/AssignCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AssignBacktracker.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AssignCont::AssignCont(ScamValue sym, Continuation * cont, Env * env)
    : EnvHelperCont(sym, cont, env, "Assign")
{
}

AssignCont *
AssignCont::makeInstance(ScamValue sym, Continuation * cont, Env * env)
{
    return new AssignCont(sym, cont, env);
}

ScamValue AssignCont::finish(ScamValue expr) const
{
    if ( isError(expr) ) {
        ScamValue test = expr->hasMeta("amb-error");
        if ( isError(test) ) {
            return test;
        }
        else if ( truth(test) ) {
            return makeNothing();
        }
    }

    ScamValue old = env->get(sym);
    if ( isUnhandledError(old) ) {
        return old;
    }

    (void) env->assign(sym, expr);

    Backtracker * backtracker = ScamEngine::getEngine().getBacktracker();
    if ( ! backtracker ) {
        return makeNothing();
    }

    Backtracker * bt =
        standardMemoryManager.make<AssignBacktracker>(sym,
                                                      old,
                                                      env,
                                                      backtracker);
    ScamEngine::getEngine().setBacktracker(bt);

    return makeNothing();
}
