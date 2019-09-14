#include "form/UndefineCont.hpp"

#include "env/Env.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

using namespace scam;
using namespace std;

UndefineCont::UndefineCont(ScamValue sym, Continuation * cont, Env * env)
    : EnvHelperCont(sym, cont, env, "Undefine")
{
}

UndefineCont *
UndefineCont::makeInstance(ScamValue sym, Continuation * cont, Env * env)
{
    return new UndefineCont(sym, cont, env);
}

ScamValue UndefineCont::finish(ScamValue expr) const
{
    ScamValue test = env->remove(sym);
    if ( isError(test) ) {
        return test;
    }
    return makeNothing();
}
