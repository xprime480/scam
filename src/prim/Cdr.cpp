#include "prim/Cdr.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"

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
    ScamValue cdr = getCdr(cons);
    cont->run(cdr);
}
