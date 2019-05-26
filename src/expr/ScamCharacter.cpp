#include "expr/ScamCharacter.hpp"

#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamCharacter::ScamCharacter(const string & value)
    : ScamData(ScamData::Character)
{
    CHARVAL(this) = 0 == value.size() ? '\0' : value[value.size() - 1];
}

ScamCharacter * ScamCharacter::makeInstance(const string & value)
{
    return new ScamCharacter(value);
}
