#include "form/Apply.hpp"

#include "expr/ScamExpr.hpp"
#include "form/ApplyOpCont.hpp"
#include "input/ApplyParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "apply";

Apply::Apply()
    : SpecialForm(myName)
{
}

Apply * Apply::makeInstance()
{
    static Apply instance;
    return &instance;
}

void Apply::apply(ExprHandle args, Continuation * cont, Env * env)
{
    ApplyParser * parser = standardMemoryManager.make<ApplyParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(function (args*))", args, cont);
        return;
    }

    ExprHandle sym     = parser->getParsedOp();
    ExprHandle arglist = const_cast<ExprHandle>(parser->getArgs());
    Continuation * newCont =
        standardMemoryManager.make<ApplyOpCont>(arglist, cont, env);

    sym->eval(newCont, env);
}
