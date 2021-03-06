#include "prim/Backtrack.hpp"

#include "Backtracker.hpp"
#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "util/Parameter.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

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
