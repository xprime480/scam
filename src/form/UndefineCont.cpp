#include "form/UndefineCont.hpp"

#include "Env.hpp"

using namespace scam;
using namespace std;

UndefineCont::UndefineCont(ScamEnvKeyType sym,
                           Continuation * cont,
                           Env * env)
    : EnvHelperCont(sym, cont, env, "Undefine")
{
}

UndefineCont * UndefineCont::makeInstance(ScamEnvKeyType sym,
                                          Continuation * cont,
                                          Env * env)
{
    return new UndefineCont(sym, cont, env);
}

void UndefineCont::finish(ScamValue expr) const
{
    env->remove(sym);
}
