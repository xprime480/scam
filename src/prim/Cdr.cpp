
#include "prim/Cdr.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Cdr::Cdr()
    : CarCdr("cdr")
{
}

Cdr * Cdr::makeInstance()
{
    return new Cdr();
}

void Cdr::finish(ScamExpr * args, Continuation * cont)
{
    ScamExpr * cdr = args->nthcar(0)->nthcdr(0);
    cont->run(cdr);
}
