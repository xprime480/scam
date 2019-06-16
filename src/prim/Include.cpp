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

void scam::applyInclude(ScamValue args,
                        Continuation * cont,
                        ScamEngine * engine)
{
    IncludeParser * parser = standardMemoryManager.make<IncludeParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(string+)", args, cont, engine);
    }
    else {
        workQueueHelper<IncludeWorker>(parser, cont, engine, 0);
    }
}
