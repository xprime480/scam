#include "form/Amb.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "form/AmbBacktracker.hpp"
#include "input/ListParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "amb";

Amb::Amb(ScamEngine * engine)
    : SpecialForm(myName, applyAmb, engine, true)
{
}

Amb * Amb::makeInstance(ScamEngine * engine)
{
    return new Amb(engine);
}

void scam::applyAmb(ScamValue args,
		    Continuation * cont,
		    Env * env,
		    ScamEngine * engine)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont);
        return;
    }

    Backtracker * backtracker = engine->getBacktracker();
    Backtracker * newBt =
        standardMemoryManager.make<AmbBacktracker>(args,
                                                   cont,
                                                   env,
                                                   engine,
                                                   backtracker);
    newBt->run();
}
