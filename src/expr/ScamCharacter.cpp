#include "expr/ScamCharacter.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamCharacter::ScamCharacter(string const & value)
    : value(value)
{
}

ScamCharacter * ScamCharacter::makeInstance(string const & value)
{
    return new ScamCharacter(value);
}

string ScamCharacter::toString() const
{
    return value;
}

bool ScamCharacter::isChar() const
{
    return true;
}

char ScamCharacter::toChar() const
{
    return value[2];
}

bool ScamCharacter::equals(ScamExpr const * expr) const
{
    if ( ! expr->isChar() ) {
        return false;
    }
    ScamCharacter const * that = dynamic_cast<ScamCharacter const *>(expr);
    return value == that->value;
}
