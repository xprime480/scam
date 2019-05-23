#include "form/Macro.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/Validations.hpp"

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

void Macro::apply(ScamValue args, Continuation * cont, Env * env)
{
    LambdaParser * lambda = standardMemoryManager.make<LambdaParser>();

    ScamValue expr = nullptr;
    if ( ! lambda->accept(args) ) {
        expr = validateClosureArgs(args, "macro");
    }
    else {
        expr = ExpressionFactory::makeClosure(lambda, env, true);
    }

    cont->run(expr);
}
