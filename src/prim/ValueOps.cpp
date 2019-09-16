#include "prim/ValueOps.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

void scam::applyValues(ScamValue args, Continuation * cont)
{
    // Anything goes here
    vector<ScamValue> values;
    while ( ! isNull(args) ) {
        values.push_back(getCar(args));
        args = getCdr(args);
    }

    ScamValue rv = makeMultiple(values);
    cont->handleMultipleValues(rv);
}
