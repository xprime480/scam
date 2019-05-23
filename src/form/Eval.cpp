#include "form/Eval.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "form/EvalCont.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "eval";

Eval::Eval()
    : SpecialForm(myName)
{
}

Eval * Eval::makeInstance()
{
    static Eval instance;
    return &instance;
}

void Eval::apply(ScamValue args, Continuation * cont, Env * env)
{
    SingletonParser * parser = getSingletonOfAnythingParser();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(expr)", args, cont);
    }
    else {
        Continuation * finisher =
            standardMemoryManager.make<EvalCont>(cont, env);
        ScamValue expr = const_cast<ScamValue>(parser->get());
        expr->eval(finisher, env);
    }
}
