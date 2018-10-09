#include "expr/ScamBoolean.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamBoolean::ScamBoolean(bool value)
    : value(value)
{
}

string ScamBoolean::toString() const
{
    if ( value ) {
        return "#t";
    }
    return "#f";
}

bool ScamBoolean::truth() const
{
    return value;
}

bool ScamBoolean::isBoolean() const
{
    return true;
}

ExprHandle ScamBoolean::clone() const
{
    return ExpressionFactory::makeBoolean(value);
}
