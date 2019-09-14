#include "prim/ValueOps.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

void scam::applyValues(ScamValue args, Continuation * cont)
{
    // Anything goes here
    ScamData::VectorData values;
    while ( ! isNull(args) ) {
        values.push_back(getCar(args));
        args = getCdr(args);
    }

    ScamValue rv = makeMultiple(values);
    cont->handleMultipleValues(rv);
}
