#include "form/Undefine.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/UndefineWorker.hpp"
#include "input/UndefineParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "undefine";

Undefine::Undefine()
    : SpecialForm(myName, applyUndefine)
{
}

Undefine * Undefine::makeInstance()
{
    static Undefine instance;
    return &instance;
}

void scam::applyUndefine(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine)
{
    UndefineParser * parser = standardMemoryManager.make<UndefineParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(sym)", args, cont);
    }
    else {
        workQueueHelper<UndefineWorker>(parser, cont, env);
    }
}
