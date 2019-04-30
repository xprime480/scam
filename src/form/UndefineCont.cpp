#include "form/UndefineCont.hpp"

#include "Env.hpp"

using namespace scam;
using namespace std;

UndefineCont::UndefineCont(ExprHandle sym,
                           Continuation * cont,
                           Env * env)
    : EnvHelperCont(sym, cont, env, "Undefine")
{
}

UndefineCont * UndefineCont::makeInstance(ExprHandle sym,
                                          Continuation * cont,
                                          Env * env)
{
    return new UndefineCont(sym, cont, env);
}

void UndefineCont::finish(ExprHandle expr) const
{
    env->remove(sym);
}
