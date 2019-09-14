#include "prim/Substitute.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "prim/Substitutor.hpp"
#include "util/Parameter.hpp"
#include "value/ScamData.hpp"
#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

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

