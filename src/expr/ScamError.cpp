#include "expr/ScamError.hpp"

#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamError::ScamError(char const * msg, bool managed)
    : ScamExpr(ScamData::Error, managed)
{
    STRVAL(this) = msg;
}

ScamError * ScamError::makeInstance(char const * msg, bool managed)
{
    return new ScamError(msg, managed);
}
