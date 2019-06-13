#include "form/UndefineCont.hpp"

#include "Env.hpp"

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

void UndefineCont::finish(ScamValue expr) const
{
    env->remove(sym);
}
