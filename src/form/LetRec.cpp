#include "form/LetRec.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "form/LetWorker.hpp"
#include "input/LetParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "and";

LetRec::LetRec()
    : SpecialForm("letrec")
{
}

LetRec * LetRec::makeInstance()
{
    static LetRec instance;
    return &instance;
}

void LetRec::apply(ExprHandle args, Continuation * cont, Env * env)
{
    LetParser * parser = standardMemoryManager.make<LetParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(((sym form)*) form*)", args, cont);
    }
    else {
        workQueueHelper<LetWorker>(parser, cont, env, true);
    }
}
