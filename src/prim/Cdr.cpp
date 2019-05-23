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

void Cdr::finish(ScamValue cons, Continuation * cont)
{
    ScamValue cdr = cons->getCdr();
    cont->run(cdr);
}
