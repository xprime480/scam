#include "expr/ScamVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamVector::ScamVector(ExprVec const & elts)
    : ScamData(ScamData::Vector)
{
    VECTOR(this) = elts;
}

ScamVector * ScamVector::makeInstance(ExprVec const & elts)
{
    return new ScamVector(elts);
}
