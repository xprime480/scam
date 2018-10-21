#include "expr/ExpressionFactory.hpp"

#include "ScamException.hpp"
#include "expr/ScamBoolean.hpp"
#include "expr/ScamCharacter.hpp"
#include "expr/ScamClosure.hpp"
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
ExpressionFactory::makeCons(ScamExpr * car, ScamExpr * cdr)
{
    ExprHandle expr = makeForm<ScamCons>(car, cdr);
    return expr;
}

ExprHandle ExpressionFactory::makeList()
{
    return makeNil();
}

ExprHandle ExpressionFactory::makeList(ScamExpr * item)
{
    return makeCons(item, makeNil().get());
}

ExprHandle ExpressionFactory::makeVector(ExprVec const & elts)
{
    ExprHandle expr = makeForm<ScamVector>(elts);
    return expr;
}

ExprHandle ExpressionFactory::makeClosure(ScamExpr *formals,
                                          ScamExpr *forms,
                                          Env & env)
{
    ExprHandle expr = makeForm<ScamClosure>(formals, forms, env);
    return expr;
}

namespace
{
    static const unsigned MAX_HANDLES = 1 << 10;
    weak_ptr<ScamExpr> HLIST[MAX_HANDLES];

    static unsigned nextHandle = 0;

    unsigned getNextHandle()
    {
        for ( unsigned i = 0 ; i < MAX_HANDLES ; ++i ) {
            unsigned h = (nextHandle + i) % MAX_HANDLES;
            if ( HLIST[h].expired() ) {
                nextHandle = h;
                return h;
            }
        }

        throw ScamException("***Internal Error:  No more handles");
        return 0u;
    }
}

ExprHandle ExpressionFactory::clone(ScamExpr const * expr)
{
    if ( ! expr ) {
        throw ScamException("Internal Error:  nullptr to clone");
    }

    unsigned h = expr->handle;
    if ( h >= MAX_HANDLES ) {
        throw ScamException("Internal Error:  invalid handle to clone");
    }

    if ( HLIST[h].expired() ) {
        throw ScamException("Internal Error:  handle is expired");
    }

    ExprHandle rv = HLIST[h].lock();
    if ( ! rv ) {
        throw ScamException("Internal Error:  handle is nullptr");
    }

    if ( rv.get() != expr ) {
        throw ScamException("Internal Error:  inconsistent clone");
    }

    return rv;
}

unsigned ExpressionFactory::getMaxHandles()
{
    return MAX_HANDLES;
}

ExprHandle ExpressionFactory::intern(ExprHandle expr)
{
    unsigned h = getNextHandle();
    expr->setHandle(h);
    HLIST[h] = expr;
    return expr;
}
