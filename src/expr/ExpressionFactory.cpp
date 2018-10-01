#include "expr/ExpressionFactory.hpp"

#include "expr/ScamError.hpp"
#include "expr/ScamBoolean.hpp"

using namespace scam;
using namespace std;

shared_ptr<ScamExpr> ExpressionFactory::makeNull()
{
    static const shared_ptr<ScamExpr> expr = make_shared<ScamExpr>();
    return expr;
}

shared_ptr<ScamExpr> ExpressionFactory::makeError(char const * msg)
{
    shared_ptr<ScamExpr> expr = make_shared<ScamError>(msg);
    return expr;
}

shared_ptr<ScamExpr> ExpressionFactory::makeError(string const & msg)
{
    shared_ptr<ScamExpr> expr = make_shared<ScamError>(msg.c_str());
    return expr;
}

shared_ptr<ScamExpr> ExpressionFactory::makeBoolean(bool value)
{
    static const shared_ptr<ScamExpr> scamT = make_shared<ScamBoolean>(true);
    static const shared_ptr<ScamExpr> scamF = make_shared<ScamBoolean>(false);

    return value ? scamT : scamF;
}
