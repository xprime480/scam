#include "form/Apply.hpp"

#include "expr/ScamExpr.hpp"
#include "form/ApplyOpCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

Apply::Apply()
    : SpecialForm("apply")
{
}

Apply * Apply::makeInstance()
{
    static Apply instance;
    return &instance;
}

void Apply::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    ScamExpr * sym     = args->nthcar(0);
    ScamExpr * arglist = args->nthcar(1);
    Continuation * newCont =
        standardMemoryManager.make<ApplyOpCont>(arglist, cont, env);

    sym->eval(newCont, env);
}
