#include "form/Apply.hpp"

#include "expr/EvalOps.hpp"
#include "expr/ScamExpr.hpp"
#include "form/ApplyOpCont.hpp"
#include "input/ApplyParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "apply";

Apply::Apply()
    : SpecialForm(myName, applyApply)
{
}

Apply * Apply::makeInstance()
{
    static Apply instance;
    return &instance;
}

void scam::applyApply(ScamValue args,
		      Continuation * cont,
		      Env * env,
		      ScamEngine * engine)
{
    ApplyParser * parser = standardMemoryManager.make<ApplyParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(function (args*))", args, cont);
        return;
    }

    ScamValue sym     = parser->getParsedOp();
    ScamValue arglist = const_cast<ScamValue>(parser->getArgs());
    Continuation * newCont =
        standardMemoryManager.make<ApplyOpCont>(arglist, cont, env);

    eval(sym, newCont, env);
}
