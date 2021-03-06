#include "value/ValueFactory.hpp"

#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "expr/ClassOps.hpp"
#include "form/SyntaxRules.hpp"
#include "input/StringCharStream.hpp"
#include "util/Cache.hpp"
#include "util/FunctionDef.hpp"
#include "util/LambdaDef.hpp"
#include "util/MemoryManager.hpp"
#include "util/NumericConverter.hpp"
#include "util/NumericUtils.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueWriter.hpp"

using namespace scam;
using namespace std;

namespace
{
    static Cache<char> charCache;

    class ValueFactoryHook : public MemoryManager::Hook
    {
    public:
        void markRoots() const override
        {
            // charCache.mark();
            // symbolCache.mark();
        }

        void releaseRoots() override
        {
            // charCache.release();
            // symbolCache.release();
        }
    };
}

void scam::initializeValueFactory(MemoryManager & mm)
{
    static ValueFactoryHook hook;
    hook.releaseRoots();
    mm.addHook(&hook);
}

ScamValue scam::makeNothing()
{
    static ScamData instance(ScamValueType::Nothing, false);
    return &instance;
}

ScamValue scam::makeNull()
{
    static ScamData instance(ScamValueType::Null, false);
    return &instance;
}

namespace
{
    static MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();

    ScamValue boolValue(bool value)
    {
        static constexpr auto myType = ScamValueType::Boolean;
        ScamValue v = mm.make<ScamData>(myType, false);
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
    static constexpr auto myType = ScamValueType::Character;

    ScamValue v = charCache.get(c);
    if ( ! v ) {
        v = mm.make<ScamData>(myType);
        v->charValue() = c;
        v = charCache.put(c, v);
    }

    return v;
}

ScamValue scam::makeString(string const & value, bool managed)
{
    static constexpr auto myType = ScamValueType::String;
    ScamValue v = mm.make<ScamData>(myType, managed);
    v->stringValue() = value;
    return v;
}

ScamValue scam::makeError(const char * msg, ExprVec & irritants)
{
    static constexpr auto myType = ScamValueType::Error;
    ScamValue v = mm.make<ScamData>(myType);
    v->errorMessage() = msg;
    v->errorIrritants() = irritants;
    return v;
}

ScamValue scam::makeSymbol(string const & value)
{
    static constexpr auto myType = ScamValueType::Symbol;
    ScamValue v = mm.make<ScamData>(myType);
    v->stringValue() = value;
    return v;
}

ScamValue scam::makeKeyword(string const & value, bool managed)
{
    static constexpr auto myType = ScamValueType::Keyword;
    ScamValue v = mm.make<ScamData>(myType, managed);
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
    ScamValue inexactNumericSingleton(ScamValueType type)
    {
        ScamValue v = mm.make<ScamData>(type, false);
        v->exactFlag() = false;
        return v;
    }
}

ScamValue scam::makeNaN()
{
    static ScamValue instance
        { inexactNumericSingleton(ScamValueType::NaN) };
    return instance;
}

ScamValue scam::makeNegInf()
{
    static ScamValue instance
        { inexactNumericSingleton(ScamValueType::NegInf) };
    return instance;
}

ScamValue scam::makePosInf()
{
    static ScamValue instance
        { inexactNumericSingleton(ScamValueType::PosInf) };
    return instance;
}

ScamValue scam::makeComplex(ScamValue real, ScamValue imag)
{
    static constexpr auto myType = ScamValueType::Complex;
    ScamValue v = mm.make<ScamData>(myType);

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
    static constexpr auto myType = ScamValueType::Real;
    ScamValue v = mm.make<ScamData>(myType);

    v->exactFlag() = exact;
    v->realValue() = value;

    return v;
}

ScamValue scam::makeRational(int num, int den, bool exact)
{
    static constexpr auto myType = ScamValueType::Rational;
    ScamValue v = mm.make<ScamData>(myType);

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
    mpz_t bigInt;
    mpz_init_set_si(bigInt, value);
    ScamValue v = makeInteger(bigInt, exact);
    mpz_clear(bigInt);
    return v;
}

ScamValue scam::makeInteger(mpz_t value, bool exact)
{
    static constexpr auto myType = ScamValueType::Integer;
    ScamValue v = mm.make<ScamData>(myType);

    v->exactFlag() = exact;
    mpz_set(v->intPart(), value);

    return v;
}

ScamValue scam::makeInteger(string const & value, bool exact)
{
    mpz_t bigInt;

    if ( 0 != mpz_init_set_str(bigInt, value.c_str(), 10) ) {
        mpz_clear(bigInt);

        ScamValue err = makeError("<%{0}> is not a valid integer constant",
                                  makeString(value));
        err->errorCategory() = readCategory;
        return err;
    }

    ScamValue v = makeInteger(bigInt, exact);
    mpz_clear(bigInt);
    return v;
}

ScamValue scam::makePair(ScamValue car, ScamValue cdr)
{
    static constexpr auto myType = ScamValueType::Pair;
    ScamValue v = mm.make<ScamData>(myType);
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
    static constexpr auto myType = ScamValueType::Vector;
    ScamValue v = mm.make<ScamData>(myType);
    v->vectorData() = elts;
    return v;
}

ScamValue scam::makeByteVector(ByteVec const & elts)
{
    static constexpr auto myType = ScamValueType::ByteVector;
    ScamValue v = mm.make<ScamData>(myType);
    v->byteVectorData() = elts;
    return v;
}

ScamValue scam::makeDict()
{
    static constexpr auto myType = ScamValueType::Dict;
    ScamValue v = mm.make<ScamData>(myType);
    return v;
}

ScamValue scam::makeDict(ExprVec const & args)
{
    ScamValue v = makeDict();

    vector<ScamValue> input = args;
    if ( 1 == (input.size() % 2) ) {
        input.push_back(makeNull());
    }

    for ( size_t idx = 0 ; idx < input.size() ; idx += 2 ) {
        ScamValue key = input[idx];
        ScamValue val = input[idx+1];
        ScamValue rv = dictPut(v, key, val);
        if ( isUnhandledError(rv) ) {
            return rv;
        }
    }

    return v;
}

ScamValue scam::makeClosure(const LambdaDef & lambda, Env * env)
{
    static constexpr auto myType = ScamValueType::Closure;
    ScamValue v = mm.make<ScamData>(myType);
    v->closureDef() = lambda;
    v->closureEnv() = env;
    return v;
}

ScamValue scam::makeClass(ClassDef & def, Env * env)
{
    static constexpr auto myType = ScamValueType::Class;
    ScamValue v = mm.make<ScamData>(myType);
    v->classDef() = def;
    v->classEnv() = env;
    return v;
}

ScamValue scam::makeClassInstance(ScamValue value, Env * env)
{
    if ( ! isClass(value) ) {
        ScamValue err =
            makeError("Cannot make instance from non-class <%{0}>", value);
        err->errorCategory() = evalCategory;
        return err;
    }

    static constexpr auto myType = ScamValueType::Instance;
    ScamValue v = mm.make<ScamData>(myType);

    Env *& priv = v->instancePrivate();
    priv = mm.make<Env>();

    Env *& local = v->instanceLocal();
    local = env->extend();

    size_t var_count = getClassVarCount(value);
    for ( size_t n = 0 ; n < var_count ; ++n ) {
        ScamValue var = getClassVar(value, n);
        ScamValue test = local->put(var, makeNull());
        if ( isError(test) ) {
            return test;
        }
    }

    size_t fun_count = getClassMethodCount(value);
    for ( size_t n = 0 ; n < fun_count ; ++n ) {
        const FunctionDef & fun = getClassMethod(value, n);
        ScamValue name = fun.fname;
        const LambdaDef & lambda = fun.lambda;

        ScamValue impl = makeClosure(lambda, local);
        ScamValue test = priv->put(name, impl);
        if ( isError(test) ) {
            return test;
        }
    }

    return v;
}

ScamValue scam::makeContinuation(Continuation * cont)
{
    static constexpr auto myType = ScamValueType::Cont;
    ScamValue v = mm.make<ScamData>(myType);
    v->contValue() = cont;
    return v;
}

ScamValue
scam::makeSpecialForm(string const & name, SfFunction func, bool managed)
{
    static constexpr auto myType = ScamValueType::SpecialForm;
    ScamValue v = mm.make<ScamData>(myType, managed);
    v->sfName()   = name;
    v->sfFunc()   = func;

    return v;
}

ScamValue
scam::makePrimitive(string const & name, PrimFunction func, bool managed)
{
    static constexpr auto myType = ScamValueType::Primitive;
    ScamValue v = mm.make<ScamData>(myType, managed);
    v->primName()   = name;
    v->primFunc()   = func;

    return v;
}

ScamValue scam::makePort(ScamPort * port)
{
    static constexpr auto myType = ScamValueType::Port;
    ScamValue v = mm.make<ScamData>(myType);
    v->portValue() = port;
    return v;
}

ScamValue scam::makeEof()
{
    static ScamData instance(ScamValueType::Eof, false);
    return &instance;
}

ScamValue scam::makeSyntax(const SyntaxRules & def)
{
    static constexpr auto myType = ScamValueType::Syntax;
    ScamValue v = mm.make<ScamData>(myType);
    v->syntaxRules() = def;
    return v;
}

ScamValue scam::makeEnv(Env * env)
{
    static constexpr auto myType = ScamValueType::ScamEnv;
    ScamValue v = mm.make<ScamData>(myType);
    v->envValue() = env;
    return v;
}

ScamValue scam::makeForwarder(Env * env)
{
    ScamValue v = makeEnv(env);
    v->setMeta("forward-address", makeNothing());
    return v;
}

ScamValue scam::makePlaceholder(ScamValue tag)
{
    static constexpr auto myType = ScamValueType::Placeholder;
    ScamValue v = mm.make<ScamData>(myType);
    v->placeholderValue() = tag;
    return v;
}

ScamValue scam::makeMultiple(ExprVec const & elts)
{
    static constexpr auto myType = ScamValueType::Multiple;
    ScamValue v = mm.make<ScamData>(myType);
    v->multipleValues() = elts;
    return v;
}

