#include "prim/Include.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "prim/IncludeWorker.hpp"
#include "input/IncludeParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "include";

Include::Include(ScamEngine * engine)
    : Primitive(myName)
    , engine(engine)
{
}

Include * Include::makeInstance(ScamEngine * engine)
{
    return new Include(engine);
}

void Include::applyArgs(ScamValue args, Continuation * cont)
{
    IncludeParser * parser = standardMemoryManager.make<IncludeParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(string+)", args, cont);
    }
    else {
        workQueueHelper<IncludeWorker>(parser, cont, engine, 0);
    }
}
