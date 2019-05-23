#include "expr/ScamString.hpp"

#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamString::ScamString(string const & value)
    : ScamExpr(ScamData::String)
{
    STRVAL(this) = value;
}

ScamString * ScamString::makeInstance(std::string const & value)
{
    return new ScamString(value);
}

bool ScamString::equals(ConstExprHandle expr) const
{
    if ( ! TypePredicates::isString(expr) ) {
        return false;
    }

    return STRVAL(this) == STRVAL(expr);
}

