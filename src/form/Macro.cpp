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

void Macro::apply(ExprHandle args, Continuation * cont, Env * env)
{
    ExprHandle expr = validateClosureArgs(args, "macro");

    if ( ! expr->error() ) {
        LambdaParser * lambda = standardMemoryManager.make<LambdaParser>();

        if ( ! lambda->accept(args) ) {
            expr =
                ExpressionFactory::makeError("Macro expected (formals form...)",
                                             "; got ",
                                             args->toString());
        }
        else {
            expr = ExpressionFactory::makeClosure(lambda, env, true);
        }
    }

    cont->run(expr);
}
