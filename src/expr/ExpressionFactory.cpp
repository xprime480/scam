#include "expr/ExpressionFactory.hpp"

#include "expr/ScamBoolean.hpp"
#include "expr/ScamCharacter.hpp"
#include "expr/ScamCons.hpp"
#include "expr/ScamError.hpp"
#include "expr/ScamFloat.hpp"
#include "expr/ScamInteger.hpp"
#include "expr/ScamNil.hpp"
#include "expr/ScamNull.hpp"
#include "expr/ScamString.hpp"
#include "expr/ScamSymbol.hpp"

using namespace scam;
using namespace std;

shared_ptr<ScamExpr> ExpressionFactory::makeNull()
{
    static const shared_ptr<ScamExpr> expr = make_shared<ScamNull>();
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

shared_ptr<ScamExpr> ExpressionFactory::makeCharacter(string const & value)
{
    shared_ptr<ScamExpr> expr = make_shared<ScamCharacter>(value);
    return expr;
}

shared_ptr<ScamExpr> ExpressionFactory::makeString(string const & value)
{
    shared_ptr<ScamExpr> expr = make_shared<ScamString>(value);
    return expr;
}

shared_ptr<ScamExpr> ExpressionFactory::makeSymbol(string const & value)
{
    shared_ptr<ScamExpr> expr = make_shared<ScamSymbol>(value);
    return expr;
}

shared_ptr<ScamExpr> ExpressionFactory::makeFloat(double value)
{
    shared_ptr<ScamExpr> expr = make_shared<ScamFloat>(value);
    return expr;
}

shared_ptr<ScamExpr> ExpressionFactory::makeInteger(int value)
{
    shared_ptr<ScamExpr> expr = make_shared<ScamInteger>(value);
    return expr;
}

shared_ptr<ScamExpr> ExpressionFactory::makeNil()
{
    static const shared_ptr<ScamExpr> nil = make_shared<ScamNil>();
    return nil;
}

shared_ptr<ScamExpr>
ExpressionFactory::makeCons(shared_ptr<ScamExpr> car, shared_ptr<ScamExpr> cdr)
{
    shared_ptr<ScamExpr> expr = make_shared<ScamCons>(car, cdr);
    return expr;
}
