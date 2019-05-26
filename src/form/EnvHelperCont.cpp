#include "form/EnvHelperCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamSymbol.hpp"

using namespace scam;
using namespace std;

EnvHelperCont::EnvHelperCont(ScamEnvKeyType sym,
                             Continuation * cont,
                             Env * env,
                             char const * name)
    : Continuation(name)
    , sym(sym)
    , env(env)
    , cont(cont)
{
}

void EnvHelperCont::mark() const
{
    if ( ! isMarked() ) {
        Continuation::mark();
        sym->mark();
        env->mark();
        cont->mark();
    }
}

void EnvHelperCont::run(ScamValue expr)
{
    finish(expr);
    cont->run(ExpressionFactory::makeNull());
}
