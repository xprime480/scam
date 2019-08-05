#include "expr/ValueFactory.hpp"

#include "Env.hpp"
#include "ScamException.hpp"
#include "expr/ClassOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "form/SyntaxRules.hpp"
#include "input/StringCharStream.hpp"
#include "util/FunctionDef.hpp"
#include "util/LambdaDef.hpp"
#include "util/MemoryManager.hpp"
#include "util/NumericConverter.hpp"
#include "util/NumericUtils.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamValue scam::makeNothing()
{
    static ScamData instance(ScamData::Nothing, false);
    return &instance;
}

ScamValue scam::makeNull()
{
    static ScamData instance(ScamData::Null, false);
    return &instance;
}

namespace
{
    ScamValue boolValue(bool value)
    {
        static constexpr auto myType = ScamData::Boolean;
        ScamValue v = standardMemoryManager.make<ScamData>(myType, false);
        v->boolValue() = value;
        return v;
    }
}

ScamValue scam::makeBoolean(bool value)
{
    static ScamValue yes { boolValue(true) };
    static ScamValue no { boolValue(false) };

    return value ? yes : no;
}

ScamValue scam::makeCharacter(const char c)
{
    static constexpr auto myType = ScamData::Character;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->charValue() = c;
    return v;
}

ScamValue scam::makeString(string const & value, bool managed)
{
    static constexpr auto myType = ScamData::String;
    ScamValue v = standardMemoryManager.make<ScamData>(myType, managed);
    v->stringValue() = value;
    return v;
}

ScamValue scam::makeError(const char * msg, ExprVec & irritants)
{
    static constexpr auto myType = ScamData::Error;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->errorMessage() = msg;
    v->errorIrritants() = irritants;
    return v;
}

ScamValue scam::makeSymbol(string const & value, bool managed)
{
    static constexpr auto myType = ScamData::Symbol;
    ScamValue v = standardMemoryManager.make<ScamData>(myType, managed);
    v->stringValue() = value;
    return v;
}

ScamValue scam::makeKeyword(string const & value, bool managed)
{
    static constexpr auto myType = ScamData::Keyword;
    ScamValue v = standardMemoryManager.make<ScamData>(myType, managed);
    v->stringValue() = value;
    return v;
}

ScamValue scam::makeNumeric(string const & value)
{
    StringCharStream stream(value);
    NumericConverter nc(stream);
    return nc.getValue();
}

namespace
{
    ScamValue inexactNumericSingleton(unsigned long type)
    {
        ScamValue v = standardMemoryManager.make<ScamData>(type, false);
        v->exactFlag() = false;
        return v;
    }
}

ScamValue scam::makeNaN()
{
    static ScamValue instance { inexactNumericSingleton(ScamData::NaN) };
    return instance;
}

ScamValue scam::makeNegInf()
{
    static ScamValue instance { inexactNumericSingleton(ScamData::NegInf) };
    return instance;
}

ScamValue scam::makePosInf()
{
    static ScamValue instance { inexactNumericSingleton(ScamData::PosInf) };
    return instance;
}

ScamValue scam::makeComplex(ScamValue real, ScamValue imag)
{
    static constexpr auto myType = ScamData::Complex;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);

    v->exactFlag() = real->exactFlag() && imag->exactFlag();

    if ( isPureComplex(real) || isPureComplex(imag) ) {
        static string msg =
            "Cannot set either part a complex number to another complex number";
        throw ScamException(msg);
    }

    v->realPart() = real;
    v->imagPart() = imag;

    return v;
}

ScamValue scam::makeReal(double value, bool exact)
{
    static constexpr auto myType = ScamData::Real;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);

    v->exactFlag() = exact;
    v->realValue() = value;

    return v;
}

ScamValue scam::makeRational(int num, int den, bool exact)
{
    static constexpr auto myType = ScamData::Rational;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);

    v->exactFlag() = exact;

    if ( den < 0 && num > 0 ) {
        den *= -1;
        num *= -1;
    }

    const int div = gcd(num, den);
    v->numPart() = num / div;
    v->denPart() = den / div;

    return v;
}

ScamValue scam::makeInteger(int value, bool exact)
{
    static constexpr auto myType = ScamData::Integer;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);

    v->exactFlag() = exact;
    v->intPart()   = value;

    return v;
}

ScamValue scam::makePair(ScamValue car, ScamValue cdr)
{
    static constexpr auto myType = ScamData::Pair;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->carValue() = car;
    v->cdrValue() = cdr;
    return v;
}

ScamValue scam::makeList()
{
    return makeNull();
}

ScamValue scam::makeList(ScamValue item)
{
    return makePair(item, makeNull());
}

ScamValue scam::makeList(const vector<ScamValue> & items)
{
    ScamValue rv = makeNull();

    for ( auto iter = items.rbegin() ; iter != items.rend() ; ++iter ) {
        rv = makePair(*iter, rv);
    }

    return rv;
}

ScamValue scam::makeVector(ExprVec const & elts)
{
    static constexpr auto myType = ScamData::Vector;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->vectorData() = elts;
    return v;
}

ScamValue scam::makeByteVector(ByteVec const & elts)
{
    static constexpr auto myType = ScamData::ByteVector;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->byteVectorData() = elts;
    return v;
}

ScamValue scam::makeDict()
{
    static constexpr auto myType = ScamData::Dict;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    return v;
}

ScamValue scam::makeDict(ExprVec const & args)
{
    ScamValue v = makeDict();

    ValVec input = args;
    if ( 1 == (input.size() % 2) ) {
        input.push_back(makeNull());
    }

    for ( size_t idx = 0 ; idx < input.size() ; idx += 2 ) {
        ScamValue key = input[idx];
        ScamValue val = input[idx+1];
        dictPut(v, key, val);
    }

    return v;
}

ScamValue scam::makeClosure(const LambdaDef & lambda, Env * env)
{
    static constexpr auto myType = ScamData::Closure;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->closureDef() = lambda;
    v->closureEnv() = env;
    return v;
}

ScamValue scam::makeClass(ClassDef & def, Env * env)
{
    static constexpr auto myType = ScamData::Class;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->classDef() = def;
    v->classEnv() = env;
    return v;
}

ScamValue scam::makeClassInstance(ScamValue value, Env * env)
{
    if ( ! isClass(value) ) {
        stringstream s;
        s << "Cannot make instance from non-class <"
          << writeValue(value) << ">";
        throw ScamException(s.str());
    }

    static constexpr auto myType = ScamData::Instance;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);

    Env *& priv = v->instancePrivate();
    priv = standardMemoryManager.make<Env>();

    Env *& local = v->instanceLocal();
    local = env->extend();

    size_t var_count = getClassVarCount(value);
    for ( size_t n = 0 ; n < var_count ; ++n ) {
        ScamValue var = getClassVar(value, n);
        local->put(var, makeNull());
    }

    size_t fun_count = getClassMethodCount(value);
    for ( size_t n = 0 ; n < fun_count ; ++n ) {
        const FunctionDef & fun = getClassMethod(value, n);
        ScamValue name = fun.fname;
        const LambdaDef & lambda = fun.lambda;

        ScamValue impl = makeClosure(lambda, local);
        priv->put(name, impl);
    }

    return v;
}

ScamValue scam::makeContinuation(Continuation * cont)
{
    static constexpr auto myType = ScamData::Cont;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->contValue() = cont;
    return v;
}

ScamValue scam::makeSpecialForm(string const & name,
                                SfFunction func,
                                ScamEngine * engine,
                                bool managed)
{
    static constexpr auto myType = ScamData::SpecialForm;
    ScamValue v = standardMemoryManager.make<ScamData>(myType, managed);
    v->sfName()   = name;
    v->sfFunc()   = func;
    v->sfEngine() = engine;

    return v;
}

ScamValue scam::makePrimitive(string const & name,
                              PrimFunction func,
                              ScamEngine * engine,
                              bool managed)
{
    static constexpr auto myType = ScamData::Primitive;
    ScamValue v = standardMemoryManager.make<ScamData>(myType, managed);
    v->primName()   = name;
    v->primFunc()   = func;
    v->primEngine() = engine;

    return v;
}

ScamValue scam::makePort(ScamPort * port)
{
    static constexpr auto myType = ScamData::Port;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->portValue() = port;
    return v;
}

ScamValue scam::makeEof()
{
    static ScamData instance(ScamData::Eof, false);
    return &instance;
}

ScamValue scam::makeSyntax(const SyntaxRules & def)
{
    static constexpr auto myType = ScamData::Syntax;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    v->syntaxRules() = def;
    return v;
}
