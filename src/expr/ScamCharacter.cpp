#include "expr/ScamCharacter.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamCharacter::ScamCharacter(const string & value)
    : value(0 == value.size() ? '\0' : value[value.size() - 1])
{
}

ScamCharacter * ScamCharacter::makeInstance(const string & value)
{
    return new ScamCharacter(value);
}

string ScamCharacter::toString() const
{
    stringstream s;
    s << "#\\" << value;
    return s.str();
}

bool ScamCharacter::isChar() const
{
    return true;
}

char ScamCharacter::toChar() const
{
    return value;
}

bool ScamCharacter::equals(ConstExprHandle expr) const
{
    if ( ! expr->isChar() ) {
        return false;
    }
    ScamCharacter const * that = dynamic_cast<ScamCharacter const *>(expr);
    return value == that->value;
}
