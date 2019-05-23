#include "expr/ScamVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
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

size_t ScamVector::length() const
{
    return VECTOR(this).size();
}

ExprHandle ScamVector::nthcar(size_t n) const
{
    if ( n >= length() ) {
        return ExpressionFactory::makeError("Requested index ",
                                            n,
                                            " of a ",
                                            length(),
                                            "-element vector");
    }

    return VECTOR(this)[n];
}

bool ScamVector::equals(ConstExprHandle expr) const
{
    if ( ! TypePredicates::isVector(expr) ) {
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
