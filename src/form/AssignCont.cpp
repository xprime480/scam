#include "form/AssignCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamExpr.hpp"
#include "form/AssignBacktracker.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

AssignCont::AssignCont(ExprHandle sym,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
    : EnvHelperCont(sym, cont, env, "Assign")
    , engine(engine)
{
}

AssignCont * AssignCont::makeInstance(ExprHandle sym,
                                      Continuation * cont,
                                      Env * env,
                                      ScamEngine * engine)
{
    return new AssignCont(sym, cont, env, engine);
}

void AssignCont::finish(ExprHandle expr) const
{
    if ( expr->error() && expr->hasMeta("amb-error") ) {
        return;
    }

    ExprHandle old = env->get(sym);
    env->assign(sym, expr);

    Backtracker * backtracker = engine->getBacktracker();
    if ( ! backtracker ) {
        return;
    }

    Backtracker * bt =
        standardMemoryManager.make<AssignBacktracker>(sym,
                                                      old,
                                                      env,
                                                      backtracker);
    engine->setBacktracker(bt);
}
