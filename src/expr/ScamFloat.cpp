
#include "expr/ScamFloat.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamFloat::ScamFloat(double value)
    : value(value)
{
}

string ScamFloat::toString() const
{
    stringstream s;
    s << value;
    return s.str();
}

bool ScamFloat::isFloat() const
{
    return true;
}

double ScamFloat::toFloat() const
{
    return value;
}

ExprHandle ScamFloat::clone()
{
    return ExpressionFactory::makeFloat(value);
}
