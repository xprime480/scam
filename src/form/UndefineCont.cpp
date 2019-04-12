#include "form/UndefineCont.hpp"

#include "Env.hpp"

using namespace scam;
using namespace std;

UndefineCont::UndefineCont(ScamExpr * sym,
                           Continuation * cont,
                           Env * env)
    : EnvHelperCont(sym, cont, env, "Undefine")
{
}

UndefineCont * UndefineCont::makeInstance(ScamExpr * sym,
                                          Continuation * cont,
                                          Env * env)
{
    return new UndefineCont(sym, cont, env);
}

void UndefineCont::finish(ScamExpr * expr) const
{
    env->remove(sym);
}
