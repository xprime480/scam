#include "expr/ScamCharacter.hpp"

#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamCharacter::ScamCharacter(const string & value)
    : ScamExpr(ScamData::Character)
{
    CHARVAL(this) = 0 == value.size() ? '\0' : value[value.size() - 1];
}

ScamCharacter * ScamCharacter::makeInstance(const string & value)
{
    return new ScamCharacter(value);
}

bool ScamCharacter::equals(ConstScamValue expr) const
{
    if ( ! isChar(expr) ) {
        return false;
    }

    return CHARVAL(this) == CHARVAL(expr);
}
