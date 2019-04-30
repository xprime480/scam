#include "form/Apply.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "form/ApplyOpCont.hpp"
#include "input/ApplyParser.hpp"
#include "util/MemoryManager.hpp"

#include "util/DebugTrace.hpp"

using namespace scam;
using namespace std;

Apply::Apply()
    : SpecialForm("apply")
{
}

Apply * Apply::makeInstance()
{
    static Apply instance;
    return &instance;
}

void Apply::apply(ExprHandle args, Continuation * cont, Env * env)
{
    scamTrace("Apply::apply", args->toString());

    ApplyParser * parser = standardMemoryManager.make<ApplyParser>();
    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("apply expects (function (args...))",
                                         "; got: ",
                                         args->toString());
        cont->run(err);
        return;
    }

    ExprHandle sym     = const_cast<ExprHandle>(parser->getParsedOp());
    ExprHandle arglist = const_cast<ExprHandle>(parser->getArgs());
    Continuation * newCont =
        standardMemoryManager.make<ApplyOpCont>(arglist, cont, env);

    sym->eval(newCont, env);
}
