#include "prim/VLen.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "util/Parameter.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

void scam::applyVLen(ScamValue args, Continuation * cont)
{
    static const char * name = "vlen";
    VectorParameter p0;
    if ( argsToParms(args, name, p0) ) {
        size_t len = length(p0.value);
        ScamValue val = makeInteger(len, true);
        cont->handleValue(val);
    }
}
