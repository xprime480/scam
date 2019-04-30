#include "prim/Substitute.hpp"

#include "Continuation.hpp"
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

void Substitute::applyArgs(ExprHandle args, Continuation * cont)
{
    if ( args->length() < 2 ) {
        ExprHandle err =
            ExpressionFactory::makeError("expected 2 args; got ",
                                         args->length());
        cont->run(err);
        return;
    }

    ExprHandle form = args->nthcar(0);
    ExprHandle dict = args->nthcar(1);
    ScamDict * answers = dynamic_cast<ScamDict *>(dict);
    if ( ! answers ) {
        ExprHandle err =
            ExpressionFactory::makeError("expected 'form dict'; got ",
                                         args->toString());
        cont->run(err);
        return;
    }

    Substitutor resolver(answers);
    ExprHandle rv = resolver.resolve_value(form);
    cont->run(rv);
}
