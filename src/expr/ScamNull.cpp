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

void ScamNull::eval(ContHandle cont, Env & env)
{
    static const string msg{ "The null type cannot be evaluated." };
    static const ExprHandle expr = ExpressionFactory::makeError(msg);
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
