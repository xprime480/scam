#include "prim/Backtrack.hpp"

#include "Backtracker.hpp"
#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyBacktrack(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name = "backtrack";
    if ( argsToParms(args, engine, name) ) {
        Backtracker * backtracker = engine->getBacktracker();
        if ( ! backtracker ) {
            ScamValue err = makeError("No current backtrack context");
            err->errorCategory() = valuesCategory;
            engine->handleError(err);
        }
        else {
            backtracker->run();
        }
    }
}
