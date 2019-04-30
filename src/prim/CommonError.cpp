#include "prim/CommonError.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;

ExprHandle scam::make_common_error(const char * text)
{
    ExprHandle msg = ExpressionFactory::makeString(text);
    ExprHandle rv  = ExpressionFactory::makeBoolean(false);
    return ExpressionFactory::makeList(rv, msg);
}
