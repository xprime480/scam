#include "expr/ScamByteVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
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

size_t ScamByteVector::length() const
{
    return BYTEVECTOR(this).size();
}

ScamValue ScamByteVector::nthcar(size_t n) const
{
    if ( n >= length() ) {
        return ExpressionFactory::makeError("Requested index ",
                                            n,
                                            " of a ",
                                            length(),
                                            "-element byteVector");
    }

    return ExpressionFactory::makeInteger(BYTEVECTOR(this)[n], true);
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
