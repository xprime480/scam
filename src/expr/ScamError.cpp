#include "expr/ScamError.hpp"

#include "expr/ExpressionFactory.hpp"

#include <string>

using namespace scam;
using namespace std;

ScamError::ScamError(char const * msg, bool managed)
    : ScamExpr(managed)
    , msg(msg)
{
    data.type = ScamData::Error;
}

ScamError * ScamError::makeInstance(char const * msg, bool managed)
{
    return new ScamError(msg, managed);
}

string ScamError::toString() const
{
    return msg;
}

bool ScamError::equals(ConstExprHandle expr) const
{
    if ( ! expr->error() ) {
        return false;
    }

    ScamError const * that = dynamic_cast<ScamError const *>(expr);
    return msg == that->msg;
}

