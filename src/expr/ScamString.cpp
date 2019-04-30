#include "expr/ScamString.hpp"

using namespace scam;
using namespace std;

ScamString::ScamString(string const & value)
    : value(value)
{
}

ScamString * ScamString::makeInstance(std::string const & value)
{
    return new ScamString(value);
}

string ScamString::toString() const
{
    return value;
}

bool ScamString::isString() const
{
    return true;
}

bool ScamString::equals(ConstExprHandle expr) const
{
    if ( ! expr->isString() ) {
        return false;
    }
    ScamString const * that = dynamic_cast<ScamString const *>(expr);
    return value == that->value;
}

