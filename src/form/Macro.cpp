
#include "form/Macro.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Macro::Macro()
    : SpecialForm("macro")
{
}

Macro * Macro::makeInstance()
{
    static Macro instance;
    return &instance;
}

void Macro::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    ScamExpr * expr = ExpressionFactory::makeClosure(args->getCar(),
                                                     args->getCdr(),
                                                     env,
                                                     true);
    cont->run(expr);
}
