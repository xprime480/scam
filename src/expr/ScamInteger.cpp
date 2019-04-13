#include "expr/ScamInteger.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamInteger::ScamInteger(int value)
    : ScamFloat(1.0 * value)
    , value(value)
{
}

ScamInteger * ScamInteger::makeInstance(int value)
{
    return new ScamInteger(value);
}

string ScamInteger::toString() const
{
    stringstream s;
    s << value;
    return s.str();
}

bool ScamInteger::isInteger() const
{
    return true;
}

int ScamInteger::toInteger() const
{
    return value;
}

bool ScamInteger::equals(ScamExpr const * expr) const
{
    if ( ! expr->isFloat() ) {
        return false;
    }
    return ( ::abs(this->toFloat() - expr->toFloat()) < 1e-9 );
}

