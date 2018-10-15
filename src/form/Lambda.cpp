
#include "form/Lambda.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Lambda::Lambda()
    : SpecialForm("lambda")
{
}

void Lambda::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    ExprHandle expr = ExpressionFactory::makeClosure(args->getCar(), args->getCdr(), env);
    cont->run(expr);
}
