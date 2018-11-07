
#include "form/Macro.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Macro::Macro()
    : SpecialForm("macro")
{
}

void Macro::apply(ScamExpr * args, ContHandle cont, Env env)
{
    ExprHandle expr = ExpressionFactory::makeClosure(args->getCar().get(),
                                                     args->getCdr().get(),
                                                     env,
                                                     true);
    cont->run(expr.get());
}
