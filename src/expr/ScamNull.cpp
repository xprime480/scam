#include "expr/ScamNull.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamNull::ScamNull()
    : ScamExpr(false)
{
    data.type = ScamData::Null;
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

void ScamNull::eval(Continuation * cont, Env * env) const
{
    static const string msg{ "The null type cannot be evaluated." };
    static ExprHandle expr = ExpressionFactory::makeError(msg, false);
    cont->run(expr);
}

bool ScamNull::equals(ConstExprHandle expr) const
{
    return false;
}
