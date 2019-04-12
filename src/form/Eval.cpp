#include "form/Eval.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "form/EvalCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

Eval::Eval()
    : SpecialForm("eval")
{
}

Eval * Eval::makeInstance()
{
    static Eval instance;
    return &instance;
}

void Eval::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    Continuation * finisher =
        standardMemoryManager.make<EvalCont>(cont, env);
    args->nthcar(0)->eval(finisher, env);
}
