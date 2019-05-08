#include "expr/ScamInteger.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamInteger::ScamInteger(int value)
    : ScamRational(value, 1)
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
