#include "form/EnvHelperCont.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

EnvHelperCont::EnvHelperCont(ScamValue sym,
                             Continuation * cont,
                             Env * env,
                             char const * name)
    : Continuation(name)
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
        ScamEngine::getEngine().handleError(test);
    }
    else {
        cont->handleValue(test);
    }
}
