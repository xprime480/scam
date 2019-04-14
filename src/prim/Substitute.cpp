#include "prim/Substitute.hpp"

#include "prim/Substitutor.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

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

void Substitute::applyArgs(ScamExpr * args, Continuation * cont)
{
    if ( args->length() < 2 ) {
        ScamExpr * err =
            ExpressionFactory::makeError("expected 2 args; got ",
                                         args->length());
        cont->run(err);
        return;
    }

    ScamExpr * form = args->nthcar(0);
    ScamExpr * dict = args->nthcar(1);
    ScamDict * answers = dynamic_cast<ScamDict *>(dict);
    if ( ! answers ) {
        ScamExpr * err =
            ExpressionFactory::makeError("expected 'form dict'; got ",
                                         args->toString());
        cont->run(err);
        return;
    }

    Substitutor resolver(answers);
    ScamExpr * rv = resolver.resolve_value(form);
    cont->run(rv);
}
