#include "expr/ScamVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamVector::ScamVector(ExprVec const & elts)
    : ScamExpr(ScamData::Vector)
{
    VECTOR(data) = elts;
}

ScamVector * ScamVector::makeInstance(ExprVec const & elts)
{
    return new ScamVector(elts);
}

void ScamVector::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        for ( auto const & e : VECTOR(data) ) {
            e->mark();
        }
    }
}

size_t ScamVector::length() const
{
    return VECTOR(data).size();
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

    return VECTOR(data)[n];
}

bool ScamVector::equals(ConstExprHandle expr) const
{
    if ( ! expr->isVector() ) {
        return false;
    }
    ScamVector const * that = dynamic_cast<ScamVector const *>(expr);
    if ( VECTOR(data).size() != VECTOR(that->data).size() ) {
        return false;
    }
    for ( size_t idx = 0 ; idx < VECTOR(data).size() ; ++idx ) {
        if ( ! VECTOR(data)[idx]->equals(VECTOR(that->data)[idx]) ) {
            return false;
        }
    }

    return true;
}
