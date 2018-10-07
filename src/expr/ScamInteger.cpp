
#include "expr/ScamInteger.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamInteger::ScamInteger(int value)
    : ScamFloat(1.0 * value)
    , value(value)
{
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

ExprHandle ScamInteger::clone()
{
    return ExpressionFactory::makeInteger(value);
}
