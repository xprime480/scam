#include "expr/ScamString.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamString::ScamString(string const & value)
    : value(value)
{
}

string ScamString::toString() const
{
    return value;
}

bool ScamString::isString() const
{
    return true;
}
