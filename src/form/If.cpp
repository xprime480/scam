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
    : SpecialForm(myName)
{
}

If * If::makeInstance()
{
    static If instance;
    return &instance;
}

void If::apply(ScamValue args, Continuation * cont, Env * env)
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
