#include "expr/ScamCons.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;

ScamCons::ScamCons(ScamValue car, ScamValue cdr)
    : ScamData(ScamData::Cons)
{
    CAR(this) = car;
    CDR(this) = cdr;
}

ScamCons * ScamCons::makeInstance(ScamValue car, ScamValue cdr)
{
    return new ScamCons(car, cdr);
}
