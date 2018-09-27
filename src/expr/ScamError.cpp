#include "expr/ScamError.hpp"

#include "ScamContext.hpp"

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

void ScamError::eval(ScamContext & context)
{
    auto expr = ExpressionFactory::makeError(msg);
    context.cont->run(expr);
}

bool ScamError::error() const
{
    return true;
}

