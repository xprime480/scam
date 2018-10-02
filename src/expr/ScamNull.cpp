#include "expr/ScamNull.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

std::string ScamNull::toString() const
{
    static const std::string null{ "null" };
    return null;
}

bool ScamNull::isNull() const
{
    return true;
}

bool ScamNull::truth() const
{
    return false;
}

shared_ptr<ScamExpr> ScamNull::clone()
{
    static const shared_ptr<ScamExpr> null = ExpressionFactory::makeNull();
    return null;
}
