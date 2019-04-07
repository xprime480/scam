#include <iostream>

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

MemoryManager ExpressionFactory::mm;

ScamNull * ExpressionFactory::makeNull()
{
    return mm.make<ScamNull>();
}

ScamError * ExpressionFactory::makeError(char const * msg)
{
    return mm.make<ScamError>(msg);
}

ScamError * ExpressionFactory::makeError(string const & msg)
{
    return mm.make<ScamError>(msg.c_str());
}

ScamBoolean * ExpressionFactory::makeBoolean(bool value)
{
    return mm.make<ScamBoolean>(value);
}

ScamCharacter * ExpressionFactory::makeCharacter(string const & value)
{
    return mm.make<ScamCharacter>(value);
}

ScamString * ExpressionFactory::makeString(string const & value)
{
    return mm.make<ScamString>(value);
}

ScamSymbol * ExpressionFactory::makeSymbol(string const & value)
{
    return mm.make<ScamSymbol>(value);
}

ScamKeyword * ExpressionFactory::makeKeyword(string const & value)
{
    return mm.make<ScamKeyword>(value);
}

ScamFloat * ExpressionFactory::makeFloat(double value)
{
    return mm.make<ScamFloat>(value);
}

ScamInteger * ExpressionFactory::makeInteger(int value)
{
    return mm.make<ScamInteger>(value);
}

ScamNil * ExpressionFactory::makeNil()
{
    return mm.make<ScamNil>();
}

ScamCons * ExpressionFactory::makeCons(ScamExpr * car, ScamExpr * cdr)
{
    return mm.make<ScamCons>(car, cdr);
}

ScamExpr * ExpressionFactory::makeList()
{
    return makeNil();
}

ScamExpr * ExpressionFactory::makeList(ScamExpr * item)
{
    return makeCons(item, makeNil());
}

ScamVector * ExpressionFactory::makeVector(ExprVec const & elts)
{
    return mm.make<ScamVector>(elts);
}

ScamClosure * ExpressionFactory::makeClosure(ScamExpr *formals,
                                             ScamExpr *forms,
                                             Env env,
                                             bool macrolike)
{
    return mm.make<ScamClosure>(formals, forms, env, macrolike);
}

ScamClass * ExpressionFactory::makeClass(ScamExpr * base,
                                         ScamExpr * vars,
                                         ScamExpr * funs,
                                         Env env)
{
    return mm.make<ScamClass>(base, vars, funs, env);
}

ScamInstance * ExpressionFactory::makeInstance(ScamExpr * vars,
					       ScamExpr * funs,
					       Env env)
{
    return mm.make<ScamInstance>(vars, funs, env);
}

ScamContinuation * ExpressionFactory::makeContinuation(ContHandle cont)
{
    return mm.make<ScamContinuation>(cont);
}

ScamDict * ExpressionFactory::makeDict()
{
    return mm.make<ScamDict>();
}

ScamDict * ExpressionFactory::makeDict(ExprVec const & args)
{
    return mm.make<ScamDict>(args);
}
