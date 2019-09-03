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

void scam::applyBacktrack(ScamValue args, Continuation * cont)
{
    static const char * name = "backtrack";
    if ( argsToParms(args, name) ) {
        Backtracker * backtracker = ScamEngine::getEngine().getBacktracker();
        if ( ! backtracker ) {
            ScamValue err = makeError("No current backtrack context");
            err->errorCategory() = valuesCategory;
            ScamEngine::getEngine().handleError(err);
        }
        else {
            backtracker->run();
        }
    }
}
