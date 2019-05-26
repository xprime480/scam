#include "form/Macro.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/Validations.hpp"

using namespace scam;
using namespace std;

Macro::Macro()
    : SpecialForm("macro", applyMacro)
{
}

Macro * Macro::makeInstance()
{
    static Macro instance;
    return &instance;
}

void scam::applyMacro(ScamValue args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine)
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
