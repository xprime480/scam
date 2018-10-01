#include "expr/ScamExpr.hpp"

#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

std::string ScamExpr::toString() const
{
    static const std::string null{ "null" };
    return null;
}

void ScamExpr::eval(ScamContext & context)
{
    static const shared_ptr<ScamExpr> null = ExpressionFactory::makeNull();
    context.cont->run(null);
}

bool ScamExpr::isNull() const
{
    return true;
}

bool ScamExpr::error() const
{
    return false;
}


