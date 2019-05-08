#include "expr/ScamReal.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamReal::ScamReal(double value, bool exact)
    : ScamComplex(value, 0.0, exact)
    , value(value)
{
}

ScamReal * ScamReal::makeInstance(double value, bool exact)
{
    return new ScamReal(value, exact);
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
