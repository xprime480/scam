#include "expr/ScamError.hpp"

#include "expr/ExpressionFactory.hpp"

#include <string>

using namespace scam;
using namespace std;

ScamError::ScamError(char const * msg)
    : msg(msg)
{
}

ScamError * ScamError::makeInstance(char const * msg)
{
    return new ScamError(msg);
}


string ScamError::toString() const
{
    return msg;
}

bool ScamError::isNull() const
{
    return false;
}

bool ScamError::error() const
{
    return true;
}

bool ScamError::equals(ScamExpr const * expr) const
{
    if ( ! expr->error() ) {
        return false;
    }

    ScamError const * that = dynamic_cast<ScamError const *>(expr);
    return msg == that->msg;
}

