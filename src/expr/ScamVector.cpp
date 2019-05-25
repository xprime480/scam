#include "expr/ScamVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamVector::ScamVector(ExprVec const & elts)
    : ScamExpr(ScamData::Vector)
{
    VECTOR(this) = elts;
}

ScamVector * ScamVector::makeInstance(ExprVec const & elts)
{
    return new ScamVector(elts);
}

bool ScamVector::equals(ConstScamValue expr) const
{
    if ( ! isVector(expr) ) {
        return false;
    }

    if ( VECTOR(this).size() != VECTOR(expr).size() ) {
        return false;
    }
    for ( size_t idx = 0 ; idx < VECTOR(this).size() ; ++idx ) {
        if ( ! VECTOR(this)[idx]->equals(VECTOR(expr)[idx]) ) {
            return false;
        }
    }

    return true;
}
