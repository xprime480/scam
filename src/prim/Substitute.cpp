#include "prim/Substitute.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "prim/Substitutor.hpp"

using namespace std;
using namespace scam;

void scam::applySubstitute(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    if ( length(args) < 2 ) {
        ScamValue err = makeError("Incorrect parameter count",
                                  makeInteger(length(args), true));
        cont->handleValue(err);
        return;
    }

    ScamValue form = nthcar(args, 0);
    ScamValue dict = nthcar(args, 1);
    if ( ! dict ) {
        ScamValue err = makeError("substitute requires dictionary", args);
        engine->handleError(err);
        return;
    }

    Substitutor resolver(dict);
    ScamValue rv = resolver.resolve_value(form);
    cont->handleValue(rv);
}

