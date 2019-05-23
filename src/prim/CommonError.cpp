#include "prim/CommonError.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;

ScamValue scam::make_common_error(const char * text)
{
    ScamValue msg = ExpressionFactory::makeString(text);
    ScamValue rv  = ExpressionFactory::makeBoolean(false);
    return ExpressionFactory::makeList(rv, msg);
}
