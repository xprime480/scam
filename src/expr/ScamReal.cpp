#include "expr/ScamReal.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamReal::ScamReal(double value, bool exact, bool managed)
    : ScamComplex(value, 0.0, exact, managed)
    , value(value)
{
}

ScamReal * ScamReal::makeInstance(double value, bool exact, bool managed)
{
    return new ScamReal(value, exact, managed);
}

string ScamReal::toString() const
{
    stringstream s;
    s << value;
    return s.str();
}

bool ScamReal::isReal() const
{
    return true;
}

double ScamReal::toReal() const
{
    return value;
}
