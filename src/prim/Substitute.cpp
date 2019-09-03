#include "prim/Substitute.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "prim/Substitutor.hpp"
#include "util/Parameter.hpp"

using namespace std;
using namespace scam;

void scam::applySubstitute(ScamValue args, Continuation * cont)
{
    ObjectParameter p0;
    DictParameter p1;
    if ( argsToParms(args, "substitute", p0, p1) ) {
        ScamValue form = p0.value;
        ScamValue dict = p1.value;

        Substitutor resolver(dict);
        ScamValue rv = resolver.resolve_value(form);
        cont->handleValue(rv);
    }
}

