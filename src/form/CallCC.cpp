#include "form/CallCC.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/CallCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

CallCC::CallCC()
    : SpecialForm("call/cc")
{
}

CallCC * CallCC::makeInstance()
{
    static CallCC instance;
    return &instance;
}

void CallCC::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    ScamExpr * body = args->nthcar(0);
    Continuation * newCont =
        standardMemoryManager.make<CallCont>(cont, env);
    body->eval(newCont, env);
}
