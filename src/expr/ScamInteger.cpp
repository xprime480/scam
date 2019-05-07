#include "expr/ScamInteger.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamInteger::ScamInteger(int value)
    : ScamReal(1.0 * value)
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

bool ScamInteger::equals(ConstExprHandle expr) const
{
    if ( ! expr->isReal() ) {
        return false;
    }
    return ( ::abs(this->toReal() - expr->toReal()) < 1e-9 );
}

