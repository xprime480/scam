#include "prim/Begin.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

void scam::applyBegin(ScamValue args, Continuation * cont)
{
    static const char * name = "begin";

    ObjectParameter itemizer;
    CountedParameter p0(itemizer);
    if ( argsToParms(args, name, p0) ) {
        ScamValue rv = p0.value;

        const size_t count = length(rv);
        if ( count > 0 ) {
            rv = nthcar(rv, count - 1);
        }

        cont->handleValue(rv);
    }
}
