#include "expr/ScamByteVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamByteVector::ScamByteVector(ByteVec const & elts)
    : ScamExpr(ScamData::ByteVector)
{
    BYTEVECTOR(data) = elts;
}

ScamByteVector * ScamByteVector::makeInstance(ByteVec const & elts)
{
    return new ScamByteVector(elts);
}

size_t ScamByteVector::length() const
{
    return BYTEVECTOR(data).size();
}

ExprHandle ScamByteVector::nthcar(size_t n) const
{
    if ( n >= length() ) {
        return ExpressionFactory::makeError("Requested index ",
                                            n,
                                            " of a ",
                                            length(),
                                            "-element byteVector");
    }

    return ExpressionFactory::makeInteger(BYTEVECTOR(data)[n], true);
}

bool ScamByteVector::equals(ConstExprHandle expr) const
{
    if ( ! expr->isByteVector() ) {
        return false;
    }
    ScamByteVector const * that = dynamic_cast<ScamByteVector const *>(expr);
    if ( BYTEVECTOR(data).size() != BYTEVECTOR(that->data).size() ) {
        return false;
    }
    for ( size_t idx = 0 ; idx < BYTEVECTOR(data).size() ; ++idx ) {
        if ( BYTEVECTOR(data)[idx] != BYTEVECTOR(that->data)[idx] ) {
            return false;
        }
    }

    return true;
}
