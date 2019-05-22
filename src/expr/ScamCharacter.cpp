#include "expr/ScamCharacter.hpp"

using namespace scam;
using namespace std;

ScamCharacter::ScamCharacter(const string & value)
    : ScamExpr(ScamData::Character)
{
    CHARVAL(data) = 0 == value.size() ? '\0' : value[value.size() - 1];
}

ScamCharacter * ScamCharacter::makeInstance(const string & value)
{
    return new ScamCharacter(value);
}

char ScamCharacter::toChar() const
{
    return CHARVAL(data);
}

bool ScamCharacter::equals(ConstExprHandle expr) const
{
    if ( ! expr->isChar() ) {
        return false;
    }
    ScamCharacter const * that = dynamic_cast<ScamCharacter const *>(expr);
    return CHARVAL(data) == CHARVAL(that->data);
}
