#include "form/And.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "form/AndWorker.hpp"
#include "input/ListParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "and";

And::And()
    : SpecialForm(myName, applyAnd)
{
}

And * And::makeInstance()
{
    static And instance;
    return &instance;
}

void scam::applyAnd(ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont);
        return;
    }

    unsigned pos { 0 };
    workQueueHelper<AndWorker>(cont, env, parser, pos);
}
