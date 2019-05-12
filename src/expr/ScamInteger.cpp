#include "expr/ScamInteger.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamInteger::ScamInteger(int value, bool exact, bool managed)
    : ScamRational(value, 1, exact, managed)
    , value(value)
{
}

ScamInteger * ScamInteger::makeInstance(int value, bool exact, bool managed)
{
    return new ScamInteger(value, exact, managed);
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
