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
