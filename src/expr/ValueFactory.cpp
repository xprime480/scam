#include "expr/ValueFactory.hpp"

#include "Env.hpp"
#include "ScamException.hpp"
#include "expr/ClassOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "input/FunctionDefParser.hpp"
#include "util/MemoryManager.hpp"
#include "util/NumericConverter.hpp"
#include "util/NumericUtils.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamValue scam::makeNull()
{
    static ScamData instance(ScamData::Null, false);
    return &instance;
}

ScamValue scam::makeNil()
{
    static ScamData instance(ScamData::Nil, false);
    return &instance;
}

namespace
{
    ScamValue boolValue(bool value)
    {
        static constexpr auto myType = ScamData::Boolean;
        ScamValue v = standardMemoryManager.make<ScamData>(myType, false);
        BOOLVAL(v) = value;
        return v;
    }
}

ScamValue scam::makeBoolean(bool value)
{
    static ScamValue yes { boolValue(true) };
    static ScamValue no { boolValue(false) };

    return value ? yes : no;
}

ScamValue scam::makeCharacter(string const & value)
{
    static constexpr auto myType = ScamData::Character;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    CHARVAL(v) = 0 == value.size() ? '\0' : value[value.size() - 1];
    return v;
}

ScamValue scam::makeString(string const & value)
{
    static constexpr auto myType = ScamData::String;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    STRVAL(v) = value;
    return v;
}


ScamValue scam::makeError(const char * msg, bool managed)
{
    string tmp(msg);
    return makeError(tmp, managed);
}

ScamValue scam::makeError(string const & msg, bool managed)
{
    static constexpr auto myType = ScamData::Error;
    ScamValue v = standardMemoryManager.make<ScamData>(myType, managed);
    STRVAL(v) = msg;
    return v;
}

ScamValue scam::makeSymbol(string const & value, bool managed)
{
    static constexpr auto myType = ScamData::Symbol;
    ScamValue v = standardMemoryManager.make<ScamData>(myType, managed);
    STRVAL(v) = value;
    return v;
}

ScamValue scam::makeKeyword(string const & value, bool managed)
{
    static constexpr auto myType = ScamData::Keyword;
    ScamValue v = standardMemoryManager.make<ScamData>(myType, managed);
    STRVAL(v) = value;
    return v;
}

ScamValue scam::makeNumeric(string const & value)
{
    NumericConverter nc(value.c_str());
    return nc.getValue();
}

namespace
{
    ScamValue inexactNumericSingleton(unsigned long type)
    {
        ScamValue v = standardMemoryManager.make<ScamData>(type, false);
        EXACT(v) = false;
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

    EXACT(v) = EXACT(real) && EXACT(imag);

    if ( isPureComplex(real) || isPureComplex(imag) ) {
        static string msg =
            "Cannot set either part a complex number to another complex number";
        throw ScamException(msg);
    }

    REALPART(v) = real;
    IMAGPART(v) = imag;

    return v;
}

ScamValue scam::makeReal(double value, bool exact)
{
    static constexpr auto myType = ScamData::Real;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);

    EXACT(v) = exact;
    REALVAL(v) = value;

    return v;
}

ScamValue scam::makeRational(int num, int den, bool exact)
{
    static constexpr auto myType = ScamData::Rational;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);

    EXACT(v) = exact;

    const int div = gcd(num, den);
    NUMPART(v) = num / div;
    DENPART(v) = den / div;

    return v;
}

ScamValue scam::makeInteger(int value, bool exact)
{
    static constexpr auto myType = ScamData::Integer;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);

    EXACT(v) = exact;
    INTVAL(v) = value;

    return v;
}

ScamValue scam::makeCons(ScamValue car, ScamValue cdr)
{
    static constexpr auto myType = ScamData::Cons;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    CAR(v) = car;
    CDR(v) = cdr;
    return v;
}

ScamValue scam::makeList()
{
    return makeNil();
}

ScamValue scam::makeList(ScamValue item)
{
    return makeCons(item, makeNil());
}

ScamValue scam::makeList(vector<ScamValue> & items)
{
    ScamValue rv = makeNil();

    for ( auto iter = items.rbegin() ; iter != items.rend() ; ++iter ) {
        rv = makeCons(*iter, rv);
    }

    return rv;
}

ScamValue scam::makeVector(ExprVec const & elts)
{
    static constexpr auto myType = ScamData::Vector;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    VECTOR(v) = elts;
    return v;
}

ScamValue scam::makeByteVector(ByteVec const & elts)
{
    static constexpr auto myType = ScamData::ByteVector;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    BYTEVECTOR(v) = elts;
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
        input.push_back(makeNil());
    }

    for ( size_t idx = 0 ; idx < input.size() ; idx += 2 ) {
        ScamValue key = input[idx];
        ScamValue val = input[idx+1];
        dictPut(v, key, val);
    }

    return v;
}

ScamValue scam::makeClosure(const LambdaParser * parser,
                            Env * env,
                            bool macrolike)
{
    static constexpr auto myType = ScamData::Closure;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    CLOSUREDEF(v) = parser;
    CLOSUREENV(v) = env;
    MACROLIKE(v) = macrolike;
    return v;
}

ScamValue scam::makeClass(ClassDefParser * def, Env * env)
{
    static constexpr auto myType = ScamData::Class;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    CLASSDEF(v) = def;
    CLASSENV(v) = env;
    return v;
}

ScamValue scam::makeClassInstance(ScamValue value, Env * env)
{
    if ( ! isClass(value) ) {
        stringstream s;
        s << "Cannot make instance from non-class <" << writeValue(value) << ">";
        throw ScamException(s.str());
    }

    static constexpr auto myType = ScamData::Instance;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);

    INSTANCEPRIVENV(v)  = standardMemoryManager.make<Env>();
    INSTANCELOCALENV(v) = env->extend();

    size_t var_count = getClassVarCount(value);
    for ( size_t n = 0 ; n < var_count ; ++n ) {
        ScamValue var = getClassVar(value, n);
        INSTANCELOCALENV(v)->put(var, makeNil());
    }

    size_t fun_count = getClassMethodCount(value);
    for ( size_t n = 0 ; n < fun_count ; ++n ) {
        const FunctionDefParser * fun = getClassMethod(value, n);

        ScamValue name = fun->getName();
        const LambdaParser * lambda = fun->getLambda();
        ScamValue impl = makeClosure(lambda, INSTANCELOCALENV(v), false);

        INSTANCEPRIVENV(v)->put(name, impl);
    }

    return v;
}

ScamValue scam::makeContinuation(Continuation * cont)
{
    static constexpr auto myType = ScamData::Cont;
    ScamValue v = standardMemoryManager.make<ScamData>(myType);
    CONTINUATION(v) = cont;
    return v;
}
