#include "form/Assign.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "form/AssignWorker.hpp"
#include "input/SymbolPlusParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "assign!";

Assign::Assign(ScamEngine * engine)
    : SpecialForm(myName, applyAssign, engine, true)
{
}

Assign * Assign::makeInstance(ScamEngine * engine)
{
    return new Assign(engine);
}

void scam::applyAssign(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    AssignParser * parser = standardMemoryManager.make<AssignParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(sym expr)", args, cont);
    }
    else {
        workQueueHelper<AssignWorker>(parser, cont, env, engine);
    }
}
