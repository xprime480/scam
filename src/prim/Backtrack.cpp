#include "prim/Backtrack.hpp"

#include "Backtracker.hpp"
#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "backtrack";

Backtrack::Backtrack(ScamEngine * engine)
    : Primitive(myName)
    , engine(engine)
{
}

Backtrack * Backtrack::makeInstance(ScamEngine * engine)
{
    return new Backtrack(engine);
}

void Backtrack::applyArgs(ScamValue args, Continuation * cont)
{
    if ( ! isNil(args) ) {
        failedArgParseMessage(myName, "()", args, cont);
        return;
    }

    Backtracker * backtracker = engine->getBacktracker();
    if ( ! backtracker ) {
        static const string msg = "No current backtrack context";
        static ScamValue rv = makeError(msg, false);
        cont->run(rv);
    }
    else {
        backtracker->run();
    }
}
