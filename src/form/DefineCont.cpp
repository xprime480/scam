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

DefineCont::DefineCont(ScamValue sym, Continuation * cont, Env * env)
    : EnvHelperCont(sym, cont, env, "Define")
{
}

DefineCont *
DefineCont::makeInstance(ScamValue sym, Continuation * cont, Env * env)
{
    return new DefineCont(sym, cont, env);
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

    Backtracker * backtracker = ScamEngine::getEngine().getBacktracker();
    if ( ! backtracker ) {
        return makeNothing();
    }

    Backtracker * bt =
        standardMemoryManager.make<DefineBacktracker>(sym, env, backtracker);
    ScamEngine::getEngine().setBacktracker(bt);

    return makeNothing();
}
