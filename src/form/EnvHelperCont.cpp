#include "form/EnvHelperCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

EnvHelperCont::EnvHelperCont(ScamValue sym,
                             Continuation * cont,
                             Env * env,
                             ScamEngine * engine,
                             char const * name)
    : Continuation(name, engine)
    , sym(sym)
    , env(env)
    , cont(cont)
{
}

void EnvHelperCont::mark()
{
    if ( ! isMarked() ) {
        Continuation::mark();
        sym->mark();
        env->mark();
        cont->mark();
    }
}

void EnvHelperCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);
    ScamValue test = finish(value);
    if ( isUnhandledError(test) ) {
        engine->handleError(test);
    }
    else {
        cont->handleValue(test);
    }
}
