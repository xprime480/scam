#include "form/Or.hpp"

#include "WorkQueue.hpp"
#include "form/OrWorker.hpp"
#include "input/ListParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

namespace scam
{
    class MemoryManager;
}

static const char * myName = "or";

Or::Or()
    : SpecialForm(myName)
{
}

Or * Or::makeInstance()
{
    static Or instance;
    return &instance;
}

void Or::apply(ExprHandle args, Continuation * cont, Env * env)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont);
    }
    else {
        workQueueHelper<OrWorker>(cont, env, parser, 0u);
    }
}
