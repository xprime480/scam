#include "expr/ScamBoolean.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamBoolean::ScamBoolean(bool value)
    : ScamExpr(ScamData::Boolean, false)
{
    BOOLVAL(data) = value;
}

ScamBoolean * ScamBoolean::makeInstance(bool value)
{
    static ScamBoolean yes(true);
    static ScamBoolean no(false);

    return value ? &yes : &no;
}

string ScamBoolean::toString() const
{
    if ( BOOLVAL(data) ) {
        return "#t";
    }
    return "#f";
}

bool ScamBoolean::equals(ConstExprHandle expr) const
{
    if ( ! expr->isBoolean() ) {
        return false;
    }
    ScamBoolean const * that = dynamic_cast<ScamBoolean const *>(expr);
    return BOOLVAL(data) == BOOLVAL(that->data);
}
