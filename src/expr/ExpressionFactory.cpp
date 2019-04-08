#include <iostream>

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamNull * ExpressionFactory::makeNull()
{
    return standardMemoryManager.make<ScamNull>();
}

ScamError * ExpressionFactory::makeError(char const * msg, bool managed)
{
    return standardMemoryManager.make<ScamError>(msg, managed);
}

ScamError * ExpressionFactory::makeError(string const & msg, bool managed)
{
    return standardMemoryManager.make<ScamError>(msg.c_str(), managed);
}

ScamBoolean * ExpressionFactory::makeBoolean(bool value)
{
    return standardMemoryManager.make<ScamBoolean>(value);
}

ScamCharacter * ExpressionFactory::makeCharacter(string const & value)
{
    return standardMemoryManager.make<ScamCharacter>(value);
}

ScamString * ExpressionFactory::makeString(string const & value)
{
    return standardMemoryManager.make<ScamString>(value);
}

ScamSymbol *
ExpressionFactory::makeSymbol(string const & value, bool managed)
{
    return standardMemoryManager.make<ScamSymbol>(value, managed);
}

ScamKeyword *
ExpressionFactory::makeKeyword(string const & value, bool managed)
{
    return standardMemoryManager.make<ScamKeyword>(value, managed);
}

ScamFloat * ExpressionFactory::makeFloat(double value)
{
    return standardMemoryManager.make<ScamFloat>(value);
}

ScamInteger * ExpressionFactory::makeInteger(int value)
{
    return standardMemoryManager.make<ScamInteger>(value);
}

ScamNil * ExpressionFactory::makeNil()
{
    return standardMemoryManager.make<ScamNil>();
}

ScamCons * ExpressionFactory::makeCons(ScamExpr * car, ScamExpr * cdr)
{
    return standardMemoryManager.make<ScamCons>(car, cdr);
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
    return standardMemoryManager.make<ScamVector>(elts);
}

ScamClosure * ExpressionFactory::makeClosure(ScamExpr *formals,
                                             ScamExpr *forms,
                                             Env env,
                                             bool macrolike)
{
    return standardMemoryManager.make<ScamClosure>(formals,
                                                   forms,
                                                   env,
                                                   macrolike);
}

ScamClass * ExpressionFactory::makeClass(ScamExpr * base,
                                         ScamExpr * vars,
                                         ScamExpr * funs,
                                         Env env)
{
    return standardMemoryManager.make<ScamClass>(base, vars, funs, env);
}

ScamInstance * ExpressionFactory::makeInstance(ScamExpr * vars,
                                               ScamExpr * funs,
                                               Env env)
{
    return standardMemoryManager.make<ScamInstance>(vars, funs, env);
}

ScamContinuation * ExpressionFactory::makeContinuation(ContHandle cont)
{
    return standardMemoryManager.make<ScamContinuation>(cont);
}

ScamDict * ExpressionFactory::makeDict()
{
    return standardMemoryManager.make<ScamDict>();
}

ScamDict * ExpressionFactory::makeDict(ExprVec const & args)
{
    return standardMemoryManager.make<ScamDict>(args);
}
