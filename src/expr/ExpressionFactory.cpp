#include <iostream>

#include "expr/ExpressionFactory.hpp"

#include "expr/ScamExprAll.hpp"

using namespace scam;
using namespace std;

MemoryManager ExpressionFactory::mm;

ScamExpr * ExpressionFactory::makeNull()
{
    return mm.make<ScamNull>();
}

ScamExpr * ExpressionFactory::makeError(char const * msg)
{
    ScamExpr * expr = makeForm<ScamError>(msg);
    return expr;
}

ScamExpr * ExpressionFactory::makeError(string const & msg)
{
    ScamExpr * expr = makeForm<ScamError>(msg.c_str());
    return expr;
}

ScamExpr * ExpressionFactory::makeBoolean(bool value)
{
    static ScamExpr * const scamT = makeForm<ScamBoolean>(true);
    static ScamExpr * const scamF = makeForm<ScamBoolean>(false);

    return value ? scamT : scamF;
}

ScamExpr * ExpressionFactory::makeCharacter(string const & value)
{
    ScamExpr * expr = makeForm<ScamCharacter>(value);
    return expr;
}

ScamExpr * ExpressionFactory::makeString(string const & value)
{
    ScamExpr * expr = makeForm<ScamString>(value);
    return expr;
}

ScamExpr * ExpressionFactory::makeSymbol(string const & value)
{
    ScamExpr * expr = makeForm<ScamSymbol>(value);
    return expr;
}

ScamExpr * ExpressionFactory::makeKeyword(string const & value)
{
    ScamExpr * expr = makeForm<ScamKeyword>(value);
    return expr;
}

ScamExpr * ExpressionFactory::makeFloat(double value)
{
    ScamExpr * expr = makeForm<ScamFloat>(value);
    return expr;
}

ScamExpr * ExpressionFactory::makeInteger(int value)
{
    ScamExpr * expr = makeForm<ScamInteger>(value);
    return expr;
}

ScamExpr * ExpressionFactory::makeNil()
{
    static ScamExpr * const nil = makeForm<ScamNil>();
    return nil;
}

ScamExpr * ExpressionFactory::makeCons(ScamExpr * car, ScamExpr * cdr)
{
    ScamExpr * expr = makeForm<ScamCons>(car, cdr);
    return expr;
}

ScamExpr * ExpressionFactory::makeList()
{
    return makeNil();
}

ScamExpr * ExpressionFactory::makeList(ScamExpr * item)
{
    return makeCons(item, makeNil());
}

ScamExpr * ExpressionFactory::makeVector(ExprVec const & elts)
{
    ScamExpr * expr = makeForm<ScamVector>(elts);
    return expr;
}

ScamExpr * ExpressionFactory::makeClosure(ScamExpr *formals,
                                          ScamExpr *forms,
                                          Env env,
                                          bool macrolike)
{
    ScamExpr * expr = makeForm<ScamClosure>(formals, forms, env, macrolike);
    return expr;
}

ScamExpr * ExpressionFactory::makeClass(ScamExpr * base,
                                        ScamExpr * vars,
                                        ScamExpr * funs,
                                        Env env)
{
    ScamExpr * expr = makeForm<ScamClass>(base, vars, funs, env);
    return expr;
}

ScamExpr * ExpressionFactory::makeInstance(ScamExpr * vars,
                                           ScamExpr * funs,
                                           Env env)
{
    ScamExpr * expr = makeForm<ScamInstance>(vars, funs, env);
    return expr;
}

ScamExpr * ExpressionFactory::makeContinuation(ContHandle cont)
{
    ScamExpr * expr = makeForm<ScamContinuation>(cont);
    return expr;
}

ScamExpr * ExpressionFactory::makeDict()
{
    ScamExpr * expr = makeForm<ScamDict>();
    return expr;
}

ScamExpr * ExpressionFactory::makeDict(ExprVec const & args)
{
    ScamExpr * expr = makeForm<ScamDict>(args);
    return expr;
}
