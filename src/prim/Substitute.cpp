#include "prim/Substitute.hpp"

#include "Continuation.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
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
    if ( args->length() < 2 ) {
        ScamValue err =
            ExpressionFactory::makeError("expected 2 args; got ",
                                         args->length());
        cont->run(err);
        return;
    }

    ScamValue form = args->nthcar(0);
    ScamValue dict = args->nthcar(1);
    ScamDict * answers = dynamic_cast<ScamDict *>(dict);
    if ( ! answers ) {
        ScamValue err =
            ExpressionFactory::makeError("expected 'form dict'; got ",
                                         ExprWriter::write(args));
        cont->run(err);
        return;
    }

    Substitutor resolver(answers);
    ScamValue rv = resolver.resolve_value(form);
    cont->run(rv);
}
