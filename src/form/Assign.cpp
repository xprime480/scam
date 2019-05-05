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
    : SpecialForm(myName, true)
    , engine(engine)
{
}

Assign * Assign::makeInstance(ScamEngine * engine)
{
    return new Assign(engine);
}

void Assign::apply(ExprHandle args, Continuation * cont, Env * env)
{
    AssignParser * parser = standardMemoryManager.make<AssignParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(sym expr)", args, cont);
    }
    else {
        workQueueHelper<AssignWorker>(parser, cont, env, engine);
    }
}
