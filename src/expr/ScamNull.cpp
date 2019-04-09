#include "expr/ScamNull.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamNull::ScamNull()
    : ScamExpr(false)
{
}

ScamNull * ScamNull::makeInstance()
{
    static ScamNull instance;
    return &instance;
}

std::string ScamNull::toString() const
{
    static const std::string null{ "null" };
    return null;
}

void ScamNull::eval(Continuation * cont, Env env)
{
    static const string msg{ "The null type cannot be evaluated." };
    static ScamExpr * expr = ExpressionFactory::makeError(msg, false);
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

bool ScamNull::equals(ScamExpr const * expr) const
{
    return false;
}

