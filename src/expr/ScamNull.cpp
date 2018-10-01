#include "expr/ScamNull.hpp"

#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

std::string ScamNull::toString() const
{
    static const std::string null{ "null" };
    return null;
}

void ScamNull::eval(ScamContext & context)
{
    static const shared_ptr<ScamExpr> null = ExpressionFactory::makeNull();
    context.cont->run(null);
}

bool ScamNull::isNull() const
{
    return true;
}

bool ScamNull::truth() const
{
    return false;
}


