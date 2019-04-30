#include "expr/ScamBoolean.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamBoolean::ScamBoolean(bool value)
    : ScamExpr(false)
    , value(value)
{
}

ScamBoolean * ScamBoolean::makeInstance(bool value)
{
    static ScamBoolean yes(true);
    static ScamBoolean no(false);

    return value ? &yes : &no;
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

bool ScamBoolean::equals(ConstExprHandle expr) const
{
    if ( ! expr->isBoolean() ) {
        return false;
    }
    ScamBoolean const * that = dynamic_cast<ScamBoolean const *>(expr);
    return value == that->value;
}
