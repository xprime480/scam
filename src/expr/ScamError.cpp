#include "expr/ScamError.hpp"

#include "expr/ExpressionFactory.hpp"

#include <string>

using namespace scam;
using namespace std;

ScamError::ScamError(char const * msg, bool managed)
    : ScamExpr(managed)
{
    data.type = ScamData::Error;
    STRVAL(data) = msg;
}

ScamError * ScamError::makeInstance(char const * msg, bool managed)
{
    return new ScamError(msg, managed);
}

string ScamError::toString() const
{
    return STRVAL(data);
}

bool ScamError::equals(ConstExprHandle expr) const
{
    if ( ! expr->error() ) {
        return false;
    }

    ScamError const * that = dynamic_cast<ScamError const *>(expr);
    return STRVAL(data) == STRVAL(that->data);
}

