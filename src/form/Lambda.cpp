
#include "form/Lambda.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Lambda::Lambda()
    : SpecialForm("lambda")
{
}

void Lambda::apply(ScamExpr * args, ContHandle cont, Env env)
{
    ExprHandle expr = ExpressionFactory::makeClosure(args->getCar().get(),
                                                     args->getCdr().get(),
                                                     env);
    cont->run(expr.get());
}
