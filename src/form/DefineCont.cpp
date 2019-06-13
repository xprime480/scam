#include "form/DefineCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "form/DefineBacktracker.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

DefineCont::DefineCont(ScamValue sym,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
    : EnvHelperCont(sym, cont, env, engine, "Define")
{
}

DefineCont * DefineCont::makeInstance(ScamValue sym,
                                      Continuation * cont,
                                      Env * env,
                                      ScamEngine * engine)
{
    return new DefineCont(sym, cont, env, engine);
}


void DefineCont::finish(ScamValue expr) const
{
    if ( error(expr) && expr->hasMeta("amb-error") ) {
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
                                                      backtracker,
                                                      engine);
    engine->setBacktracker(bt);
}
