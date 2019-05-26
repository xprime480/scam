#include "prim/Substitute.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "prim/Substitutor.hpp"

using namespace std;
using namespace scam;

Substitute::Substitute()
    : Primitive("substitute")
{
}

Substitute * Substitute::makeInstance()
{
    return new Substitute();
}

void Substitute::applyArgs(ScamValue args, Continuation * cont)
{
    if ( length(args) < 2 ) {
        ScamValue err = makeError("expected 2 args; got ", length(args));
        cont->run(err);
        return;
    }

    ScamValue form = nthcar(args, 0);
    ScamValue dict = nthcar(args, 1);
    if ( ! dict ) {
        ScamValue err =
            makeErrorExtended("expected 'form dict'; got ", writeValue(args));
        cont->run(err);
        return;
    }

    Substitutor resolver(dict);
    ScamValue rv = resolver.resolve_value(form);
    cont->run(rv);
}
