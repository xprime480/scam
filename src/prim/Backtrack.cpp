#include "prim/Backtrack.hpp"

#include "Backtracker.hpp"
#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

void scam::applyBacktrack(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * myName = "backtrack";

    if ( ! isNull(args) ) {
        failedArgParseMessage(myName, "()", args, cont);
        return;
    }

    Backtracker * backtracker = engine->getBacktracker();
    if ( ! backtracker ) {
        static const string msg = "No current backtrack context";
        static ScamValue rv = makeError(msg, false);
        cont->handleValue(rv);
    }
    else {
        backtracker->run();
    }
}
