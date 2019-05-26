#include "expr/ScamBoolean.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamBoolean::ScamBoolean(bool value)
    : ScamData(ScamData::Boolean, false)
{
    BOOLVAL(this) = value;
}

ScamBoolean * ScamBoolean::makeInstance(bool value)
{
    static ScamBoolean yes(true);
    static ScamBoolean no(false);

    return value ? &yes : &no;
}
