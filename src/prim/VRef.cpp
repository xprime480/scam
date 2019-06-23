#include "prim/VRef.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ScamToInternal.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyVRef(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * name = "vref";
    CountParameter p0;
    VectorParameter p1;
    if ( argsToParms(args, engine, name, p0, p1) ) {
        size_t idx = asInteger(p0.value);
        ScamValue vec = p1.value;
        cont->handleValue(nthcar(vec, idx));
    }
}
