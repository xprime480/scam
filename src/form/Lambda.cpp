
#include "form/Lambda.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Lambda::Lambda()
    : SpecialForm("lambda")
{
}

Lambda * Lambda::makeInstance()
{
    static Lambda instance;
    return &instance;
}

void Lambda::apply(ScamExpr * args, ContHandle cont, Env env)
{
    ScamExpr * expr =
        ExpressionFactory::makeClosure(args->getCar(), args->getCdr(), env);
    cont->run(expr);
}
