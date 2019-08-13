#include "form/UndefineCont.hpp"

#include "env/Env.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;

UndefineCont::UndefineCont(ScamValue sym,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
    : EnvHelperCont(sym, cont, env, engine, "Undefine")
{
}

UndefineCont * UndefineCont::makeInstance(ScamValue sym,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine)
{
    return new UndefineCont(sym, cont, env, engine);
}

ScamValue UndefineCont::finish(ScamValue expr) const
{
    ScamValue test = env->remove(sym);
    if ( isError(test) ) {
        return test;
    }
    return makeNothing();
}
