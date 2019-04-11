#include "prim/CommonError.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;

ScamExpr * make_common_error(const char * text)
{
    ScamExpr * msg = ExpressionFactory::makeString(text);
    ScamExpr * rv  = ExpressionFactory::makeBoolean(false);
    return ExpressionFactory::makeList(rv, msg);
}
