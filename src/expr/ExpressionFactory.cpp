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
    static const ExprHandle expr = makeForm<ScamNull>();
    return expr;
}

ExprHandle ExpressionFactory::makeError(char const * msg)
{
    ExprHandle expr = makeForm<ScamError>(msg);
    return expr;
}

ExprHandle ExpressionFactory::makeError(string const & msg)
{
    ExprHandle expr = makeForm<ScamError>(msg.c_str());
    return expr;
}

ExprHandle ExpressionFactory::makeBoolean(bool value)
{
    static const ExprHandle scamT = makeForm<ScamBoolean>(true);
    static const ExprHandle scamF = makeForm<ScamBoolean>(false);

    return value ? scamT : scamF;
}

ExprHandle ExpressionFactory::makeCharacter(string const & value)
{
    ExprHandle expr = makeForm<ScamCharacter>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeString(string const & value)
{
    ExprHandle expr = makeForm<ScamString>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeSymbol(string const & value)
{
    ExprHandle expr = makeForm<ScamSymbol>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeFloat(double value)
{
    ExprHandle expr = makeForm<ScamFloat>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeInteger(int value)
{
    ExprHandle expr = makeForm<ScamInteger>(value);
    return expr;
}

ExprHandle ExpressionFactory::makeNil()
{
    static const ExprHandle nil = makeForm<ScamNil>();
    return nil;
}

ExprHandle
ExpressionFactory::makeCons(ExprHandle const & car, ExprHandle const & cdr)
{
    ExprHandle expr = makeForm<ScamCons>(car, cdr);
    return expr;
}

ExprHandle ExpressionFactory::makeVector(ExprVec const & elts)
{
    ExprHandle expr = makeForm<ScamVector>(elts);
    return expr;
}
