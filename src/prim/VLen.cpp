#include "prim/VLen.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyVLen(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * name = "vlen";
    VectorParameter p0;
    if ( argsToParms(args, engine, name, p0) ) {
        size_t len = length(p0.value);
        ScamValue val = makeInteger(len, true);
        cont->handleValue(val);
    }
}
