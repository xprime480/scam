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
    : SpecialForm(myName)
{
}

And * And::makeInstance()
{
    static And instance;
    return &instance;
}

void And::apply(ExprHandle args, Continuation * cont, Env * env)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont);
        return;
    }

    unsigned pos { 0 };
    workQueueHelper<AndWorker>(cont, env, parser, pos);
}
