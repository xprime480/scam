#include "form/EnvHelperCont.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"

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
    cont->run(makeNothing());
}
