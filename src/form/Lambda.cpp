#include "form/Lambda.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/LambdaParser.hpp"
#include "util/MemoryManager.hpp"
#include "util/Validations.hpp"

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

void Lambda::apply(ExprHandle args, Continuation * cont, Env * env)
{
    LambdaParser * lambda = standardMemoryManager.make<LambdaParser>();

    ExprHandle expr = nullptr;
    if ( ! lambda->accept(args) ) {
        expr = validateClosureArgs(args, "lambda");
    }
    else {
        expr = ExpressionFactory::makeClosure(lambda, env);
    }

    cont->run(expr);
}
