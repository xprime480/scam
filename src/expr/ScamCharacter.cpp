#include "expr/ScamCharacter.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamCharacter::ScamCharacter(string const & value)
    : value(value)
{
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

shared_ptr<ScamExpr> ScamCharacter::clone()
{
    return ExpressionFactory::makeCharacter(value);
}
