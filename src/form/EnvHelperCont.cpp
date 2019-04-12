#include "form/EnvHelperCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

EnvHelperCont::EnvHelperCont(ScamExpr * sym,
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

void EnvHelperCont::run(ScamExpr * expr)
{
    finish(expr);
    cont->run(sym);
}
