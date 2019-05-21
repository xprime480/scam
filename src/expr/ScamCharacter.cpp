#include "expr/ScamCharacter.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

#define CHARVAL(data) ((data).value.charValue)

ScamCharacter::ScamCharacter(const string & value)
{
    data.type = ScamData::Character;
    CHARVAL(data) = 0 == value.size() ? '\0' : value[value.size() - 1];
}

ScamCharacter * ScamCharacter::makeInstance(const string & value)
{
    return new ScamCharacter(value);
}

string ScamCharacter::toString() const
{
    stringstream s;
    s << "#\\" << CHARVAL(data);
    return s.str();
}

bool ScamCharacter::isChar() const
{
    return true;
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
