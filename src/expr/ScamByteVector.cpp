#include "expr/ScamByteVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamByteVector::ScamByteVector(ByteVec const & elts)
    : ScamExpr(ScamData::ByteVector)
{
    BYTEVECTOR(this) = elts;
}

ScamByteVector * ScamByteVector::makeInstance(ByteVec const & elts)
{
    return new ScamByteVector(elts);
}

bool ScamByteVector::equals(ConstScamValue expr) const
{
    if ( ! isByteVector(expr) ) {
        return false;
    }

    if ( BYTEVECTOR(this).size() != BYTEVECTOR(expr).size() ) {
        return false;
    }
    for ( size_t idx = 0 ; idx < BYTEVECTOR(expr).size() ; ++idx ) {
        if ( BYTEVECTOR(this)[idx] != BYTEVECTOR(expr)[idx] ) {
            return false;
        }
    }

    return true;
}
