#include "form/Let.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/LetWorker.hpp"
#include "input/LetParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "let";

Let::Let()
    : SpecialForm(myName, applyLet)
{
}

Let * Let::makeInstance()
{
    static Let instance;
    return &instance;
}

void scam::applyLet(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(((sym form)*) form*)", args, cont);
    }
    else {
        workQueueHelper<LetWorker>(parser, cont, env, false);
    }
}
