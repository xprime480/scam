#include "expr/ScamBoolean.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamBoolean::ScamBoolean(bool value)
    : ScamExpr(ScamData::Boolean, false)
{
    BOOLVAL(this) = value;
}

ScamBoolean * ScamBoolean::makeInstance(bool value)
{
    static ScamBoolean yes(true);
    static ScamBoolean no(false);

    return value ? &yes : &no;
}

bool ScamBoolean::equals(ConstExprHandle expr) const
{
    if ( ! expr->isBoolean() ) {
        return false;
    }

    return BOOLVAL(this) == BOOLVAL(expr);
}
