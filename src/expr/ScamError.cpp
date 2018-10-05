#include "expr/ScamError.hpp"

#include "expr/ExpressionFactory.hpp"

#include <string>

using namespace scam;
using namespace std;

ScamError::ScamError(char const * msg)
    : msg(msg)
{
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

shared_ptr<ScamExpr> ScamError::clone()
{
    return ExpressionFactory::makeError(msg);
}
