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
#include "expr/ScamVector.hpp"

using namespace scam;
using namespace std;

ExprHandle ExpressionFactory::makeNull()
{
    static const ExprHandle expr = make_shared<ScamNull>();
    return expr;
}

ExprHandle ExpressionFactory::makeError(char const * msg)
{
    ExprHandle expr = make_shared<ScamError>(msg);
    return expr;
}

ExprHandle ExpressionFactory::makeError(string const & msg)
{
    ExprHandle expr = make_shared<ScamError>(msg.c_str());
    return expr;
}

ExprHandle ExpressionFactory::makeBoolean(bool value)
{
    static const ExprHandle scamT = make_shared<ScamBoolean>(true);
    static const ExprHandle scamF = make_shared<ScamBoolean>(false);

    return value ? scamT : scamF;
}

ExprHandle ExpressionFactory::makeCharacter(string const & value)
{
    ExprHandle expr = make_shared<ScamCharacter>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeString(string const & value)
{
    ExprHandle expr = make_shared<ScamString>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeSymbol(string const & value)
{
    ExprHandle expr = make_shared<ScamSymbol>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeFloat(double value)
{
    ExprHandle expr = make_shared<ScamFloat>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeInteger(int value)
{
    ExprHandle expr = make_shared<ScamInteger>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeNil()
{
    static const ExprHandle nil = make_shared<ScamNil>();
    return nil;
}

ExprHandle
ExpressionFactory::makeCons(ExprHandle const & car, ExprHandle const & cdr)
{
    ExprHandle expr = make_shared<ScamCons>(car, cdr);
    return expr;
}

ExprHandle ExpressionFactory::makeVector(ExprVec const & elts)
{
    ExprHandle expr = make_shared<ScamVector>(elts);
    return expr;
}
