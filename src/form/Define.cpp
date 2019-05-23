#include "form/Define.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "form/DefineWorker.hpp"
#include "input/SymbolPlusParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "define";

Define::Define(ScamEngine * engine)
    : SpecialForm(myName, true)
    , engine(engine)
{
}

Define * Define::makeInstance(ScamEngine * engine)
{
    return new Define(engine);
}

void Define::apply(ScamValue args, Continuation * cont, Env * env)
{
    DefineParser * parser = standardMemoryManager.make<DefineParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(sym expr)", args, cont);
    }
    else {
        workQueueHelper<DefineWorker>(parser, cont, env, engine);
    }
}
