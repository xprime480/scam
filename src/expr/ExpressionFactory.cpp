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

ScamNumeric * ExpressionFactory::makeNumeric(string const & value)
{
    return ScamNumeric::makeValue(value);
}

ScamReal * ExpressionFactory::makeReal(double value, bool exact)
{
    return standardMemoryManager.make<ScamReal>(value, exact);
}

ScamInteger * ExpressionFactory::makeInteger(int value, bool exact)
{
    return standardMemoryManager.make<ScamInteger>(value, exact);
}

ScamNil * ExpressionFactory::makeNil()
{
    return standardMemoryManager.make<ScamNil>();
}

ScamCons * ExpressionFactory::makeCons(ExprHandle car, ExprHandle cdr)
{
    return standardMemoryManager.make<ScamCons>(car, cdr);
}

ExprHandle ExpressionFactory::makeList()
{
    return makeNil();
}

ExprHandle ExpressionFactory::makeList(ExprHandle item)
{
    return makeCons(item, makeNil());
}

ExprHandle ExpressionFactory::makeList(std::vector<ExprHandle> & items)
{
    ExprHandle rv = makeNil();

    for ( auto iter = items.rbegin() ; iter != items.rend() ; ++iter ) {
        rv = makeCons(*iter, rv);
    }

    return rv;
}

ScamVector * ExpressionFactory::makeVector(ExprVec const & elts)
{
    return standardMemoryManager.make<ScamVector>(elts);
}

ScamClosure * ExpressionFactory::makeClosure(const LambdaParser * parser,
                                             Env * env,
                                             bool macrolike)
{
    return standardMemoryManager.make<ScamClosure>(parser, env, macrolike);
}

ScamClass * ExpressionFactory::makeClass(ClassDefParser * def, Env * env)
{
    return standardMemoryManager.make<ScamClass>(def, env);
}

ScamInstance * ExpressionFactory::makeInstance(const ScamClass * cls, Env * env)
{
    return standardMemoryManager.make<ScamInstance>(cls, env);
}

ScamContinuation * ExpressionFactory::makeContinuation(Continuation * cont)
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
