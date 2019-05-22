#include "expr/ScamError.hpp"

#include "expr/ExpressionFactory.hpp"

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

bool ScamError::equals(ConstExprHandle expr) const
{
    if ( ! expr->error() ) {
        return false;
    }

    return STRVAL(this) == STRVAL(expr);
}

