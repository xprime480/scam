#include "form/DefineCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamExpr.hpp"
#include "form/DefineBacktracker.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

DefineCont::DefineCont(ScamEnvKeyType sym,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
    : EnvHelperCont(sym, cont, env, "Define")
    , engine(engine)
{
}

DefineCont * DefineCont::makeInstance(ScamEnvKeyType sym,
                                      Continuation * cont,
                                      Env * env,
                                      ScamEngine * engine)
{
    return new DefineCont(sym, cont, env, engine);
}


void DefineCont::finish(ExprHandle expr) const
{
    if ( expr->error() && expr->hasMeta("amb-error") ) {
        return;
    }

    env->put(sym, expr);

    Backtracker * backtracker = engine->getBacktracker();
    if ( ! backtracker ) {
        return;
    }

    Backtracker * bt =
        standardMemoryManager.make<DefineBacktracker>(sym,
                                                      env,
                                                      backtracker);
    engine->setBacktracker(bt);
}
