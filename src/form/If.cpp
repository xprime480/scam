#include "form/If.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "form/IfWorker.hpp"
#include "input/CountedListParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "if";

If::If()
    : SpecialForm(myName, applyIf)
{
}

If * If::makeInstance()
{
    static If instance;
    return &instance;
}

void scam::applyIf(ScamValue args,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine)
{
    CountedListParser * parser = getCountedListOfAnythingParser(2, 3);

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName,
                              "(test then-expr else-expr?)",
                              args,
                              cont);
    }
    else {
        workQueueHelper<IfWorker>(cont, env, parser);
    }
}
