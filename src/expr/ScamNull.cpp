#include "expr/ScamNull.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamNull::ScamNull()
    : ScamExpr(ScamData::Null, false)
{
}

ScamNull * ScamNull::makeInstance()
{
    static ScamNull instance;
    return &instance;
}

void ScamNull::eval(Continuation * cont, Env * env) const
{
    static const string msg{ "The null type cannot be evaluated." };
    static ScamValue expr = ExpressionFactory::makeError(msg, false);
    cont->run(expr);
}

bool ScamNull::equals(ConstScamValue expr) const
{
    return false;
}
