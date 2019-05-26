#include "form/Not.hpp"

#include "WorkQueue.hpp"
#include "input/SingletonParser.hpp"
#include "form/NotWorker.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = { "not" };

namespace scam
{
    class MemoryManager;
}

Not::Not()
    : SpecialForm(myName, applyNot)
{
}

Not * Not::makeInstance()
{
    static Not instance;
    return &instance;
}

void scam::applyNot(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont);
    }
    else {
        workQueueHelper<NotWorker>(cont, env, parser);
    }
}
