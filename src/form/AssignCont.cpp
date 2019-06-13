 #include "form/AssignCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/AssignBacktracker.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AssignCont::AssignCont(ScamValue sym,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
    : EnvHelperCont(sym, cont, env, engine, "Assign")
{
}

AssignCont * AssignCont::makeInstance(ScamValue sym,
                                      Continuation * cont,
                                      Env * env,
                                      ScamEngine * engine)
{
    return new AssignCont(sym, cont, env, engine);
}

void AssignCont::finish(ScamValue expr) const
{
    if ( error(expr) && expr->hasMeta("amb-error") ) {
        return;
    }

    ScamValue old = env->get(sym);
    env->assign(sym, expr);

    Backtracker * backtracker = engine->getBacktracker();
    if ( ! backtracker ) {
        return;
    }

    Backtracker * bt =
        standardMemoryManager.make<AssignBacktracker>(sym,
                                                      old,
                                                      env,
                                                      backtracker,
                                                      engine);
    engine->setBacktracker(bt);
}
