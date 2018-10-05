#include "expr/ScamNull.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

std::string ScamNull::toString() const
{
    static const std::string null{ "null" };
    return null;
}

void ScamNull::eval(std::shared_ptr<Continuation> cont, Env & env)
{
    static const string msg{ "The null type cannot be evaluated." };
    static const shared_ptr<ScamExpr> expr = ExpressionFactory::makeError(msg);
    cont->run(expr);
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
