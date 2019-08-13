#include "form/DefineCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
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


ScamValue DefineCont::finish(ScamValue expr) const
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

    ScamValue test = env->put(sym, expr);
    if ( isError(test) ) {
        return test;
    }

    Backtracker * backtracker = engine->getBacktracker();
    if ( ! backtracker ) {
        return makeNothing();
    }

    Backtracker * bt =
        standardMemoryManager.make<DefineBacktracker>(sym,
                                                      env,
                                                      backtracker,
                                                      engine);
    engine->setBacktracker(bt);

    return makeNothing();
}
