#include "form/Lambda.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/LambdaParser.hpp"
#include "util/MemoryManager.hpp"
#include "util/Validations.hpp"

using namespace scam;
using namespace std;

Lambda::Lambda()
    : SpecialForm("lambda", applyLambda)
{
}

Lambda * Lambda::makeInstance()
{
    static Lambda instance;
    return &instance;
}

void scam::applyLambda(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    LambdaParser * lambda = standardMemoryManager.make<LambdaParser>();

    ScamValue expr = nullptr;
    if ( ! lambda->accept(args) ) {
        expr = validateClosureArgs(args, "lambda");
    }
    else {
        expr = ExpressionFactory::makeClosure(lambda, env);
    }

    cont->run(expr);
}
