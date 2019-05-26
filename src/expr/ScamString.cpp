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
