#include "expr/ScamInteger.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamInteger::ScamInteger(int value, bool exact)
    : ScamRational(value, 1, exact)
    , value(value)
{
}

ScamInteger * ScamInteger::makeInstance(int value, bool exact)
{
    return new ScamInteger(value, exact);
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
