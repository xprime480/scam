#include "expr/ScamReal.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamReal::ScamReal(double value)
    : ScamComplex(value, 0.0)
    , value(value)
{
}

ScamReal * ScamReal::makeInstance(double value)
{
    return new ScamReal(value);
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
