#include "TestBase.hpp"

#include "Env.hpp"
#include "ScamException.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AllSpecialForms.hpp"

using namespace std;
using namespace scam;

class ValueFactoryTest : public TestBase
{
protected:
    ValueFactoryTest()
        : TestBase(false)
    {
    }
};

TEST_F(ValueFactoryTest, NullExpression)
{
    ScamValue expr = makeNothing();
    expectNothing(expr);

    ScamValue evaled = evaluate(expr);
    expectError(evaled, "The null type cannot be evaluated.");
}

TEST_F(ValueFactoryTest, ErrorExpressionNoIrritants)
{
    const char * msg { "Test message" };

    ScamValue expr = makeError(msg);
    expectError(expr, msg);

    ScamValue evaled = evaluate(expr);
    expectError(evaled, msg);
}

TEST_F(ValueFactoryTest, ErrorExpressionOneIrritant)
{
    const char * msg { "Test message %{0}" };
    const char * out { "Test message 99" };

    ScamValue v = makeInteger(99, true);
    ScamValue expr = makeError(msg, v);
    expectError(expr, out);
}

TEST_F(ValueFactoryTest, ErrorExpressionNotEnoughIrritants)
{
    const char * msg { "Test message %{0} %{1}" };
    const char * out { "Test message 99 ?" };

    ScamValue v = makeInteger(99, true);
    ScamValue expr = makeError(msg, v);
    expectError(expr, out);
}

TEST_F(ValueFactoryTest, ErrorExpressionBadFormatter)
{
    const char * msg { "Test message %{only-digits}" };
    const char * out { "Test message " };

    ScamValue v = makeInteger(99, true);
    ScamValue expr = makeError(msg, v);
    expectError(expr, out);
}

TEST_F(ValueFactoryTest, BooleanTrue)
{
    ScamValue expr = makeBoolean(true);
    booleanTest(expr, true, "#t");
}

TEST_F(ValueFactoryTest, BooleanFalse)
{
    ScamValue expr = makeBoolean(false);
    booleanTest(expr, false, "#f");
}

TEST_F(ValueFactoryTest, ComplexTest)
{
    string const repr{ "6-i" };

    ScamValue real = makeReal(6, true);
    ScamValue imag = makeReal(-1, true);
    ScamValue expr = makeComplex(real, imag);
    expectComplex(expr, real, imag, repr, true);

    ScamValue evaled = evaluate(expr);
    expectComplex(evaled, real, imag, repr, true);
}

TEST_F(ValueFactoryTest, RealTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    ScamValue expr = makeReal(value, false);
    expectReal(expr, value, repr, false);

    ScamValue evaled = evaluate(expr);
    expectReal(evaled, value, repr, false);
}

TEST_F(ValueFactoryTest, RationalTest)
{
    RationalPair value { 8, 3 };
    string const repr{ "8/3" };

    ScamValue expr = makeRational(8, 3, true);
    expectRational(expr, value, repr, true);

    ScamValue evaled = evaluate(expr);
    expectRational(evaled, value, repr, true);
}

TEST_F(ValueFactoryTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    ScamValue expr = makeInteger(value, true);
    expectInteger(expr, value, repr, true);

    ScamValue evaled = evaluate(expr);
    expectInteger(evaled, value, repr, true);
}

TEST_F(ValueFactoryTest, CharacterTest)
{
    string const repr { "#\\Q" };
    char value { 'Q' };

    ScamValue expr = makeCharacter(value);
    expectChar(expr, value, repr);

    ScamValue evaled = evaluate(expr);
    expectChar(evaled, value, repr);
}

TEST_F(ValueFactoryTest, StringTest)
{
    string const value { "Fnord!" };
    string const repr { "\"Fnord!\"" };

    ScamValue expr = makeString(value);
    expectString(expr, repr);

    ScamValue evaled = evaluate(expr);
    expectString(evaled, repr);
}

TEST_F(ValueFactoryTest, SymbolTest)
{
    string const name { "Fnord!" };

    ScamValue sym = makeSymbol(name);
    expectSymbol(sym, name);

    ScamValue evaled = evaluate(sym);
    expectError(evaled);

    ScamValue value = makeInteger(1899, true);
    expectNothing(engine.addBinding(sym, value));
    evaled = evaluate(sym);
    expectInteger(evaled, 1899, "1899", true);
}

TEST_F(ValueFactoryTest, NilTest)
{
    ScamValue expr = makeNull();
    expectNull(expr);

    ScamValue evaled = evaluate(expr);
    expectNull(evaled);
}

TEST_F(ValueFactoryTest, PairSingletonTest)
{
    string const value { "(works)" };

    ScamValue car = makeSymbol("works");
    ScamValue cdr = makeNull();
    ScamValue expr = makePair(car, cdr);

    expectList(expr, value, 1);

    ScamValue first = nthcar(expr, 0);
    expectSymbol(first, "works");
}

TEST_F(ValueFactoryTest, ConsDoubletonTest)
{
    string const value { "(works also)" };

    ScamValue car  = makeSymbol("works");
    ScamValue cadr = makeSymbol("also");
    ScamValue cddr = makeNull();
    ScamValue cdr  = makePair(cadr, cddr);;
    ScamValue expr = makePair(car, cdr);

    expectList(expr, value, 2);

    ScamValue first = nthcar(expr, 0);
    expectSymbol(first, "works");

    ScamValue second = nthcar(expr, 1);
    expectSymbol(second, "also");

    ScamValue third = nthcar(expr, 2);
    expectError(third);

    ScamValue car2 = getCar(expr);
    expectSymbol(car2, "works");

    ScamValue cdr2 = getCdr(expr);
    expectList(cdr2, "(also)", 1);

    ScamValue cdr3 = nthcdr(expr, 1);
    expectNull(cdr3);
}

TEST_F(ValueFactoryTest, ListEmptyTest)
{
    ScamValue expr = makeList();
    expectNull(expr);

    ScamValue evaled = evaluate(expr);
    expectNull(evaled);
}

TEST_F(ValueFactoryTest, ListSingletonTest)
{
    string const value { "(works)" };

    ScamValue car = makeSymbol("works");
    ScamValue expr = makeList(car);

    expectList(expr, value, 1);

    ScamValue first = nthcar(expr, 0);
    expectSymbol(first, "works");
}

TEST_F(ValueFactoryTest, ListDoubletonTest)
{
    string const value { "(works also)" };

    ScamValue car0  = makeSymbol("works");
    ScamValue car1 = makeSymbol("also");
    ScamValue expr = makeList(car0, car1);

    expectList(expr, value, 2);

    ScamValue first = nthcar(expr, 0);
    expectSymbol(first, "works");

    ScamValue second = nthcar(expr, 1);
    expectSymbol(second, "also");

    ScamValue third = nthcar(expr, 2);
    expectError(third);

    ScamValue car2 = getCar(expr);
    expectSymbol(car2, "works");

    ScamValue cdr2 = getCdr(expr);
    expectList(cdr2, "(also)", 1);
}

TEST_F(ValueFactoryTest, ConsDottedPair)
{
    string const value { "(1 . 2)" };

    ScamValue car = makeInteger(1, true);
    ScamValue cdr = makeInteger(2, true);
    ScamValue expr = makePair(car, cdr);

    expectPair(expr, value);

    expectInteger(getCar(expr), 1, "1", true);
    expectInteger(getCdr(expr), 2, "2", true);
}

TEST_F(ValueFactoryTest, ConsEvalTest)
{
    string const value { "(quote 2)" };

    ScamValue car  = makeSymbol("quote");
    ScamValue cadr = makeInteger(2, true);
    ScamValue cddr = makeNull();
    ScamValue cdr  = makePair(cadr, cddr);;
    ScamValue expr = makePair(car, cdr);

    expectList(expr, value, 2);
    expectSymbol(getCar(expr), "quote");

    ScamValue evaled = evaluate(expr);
    expectInteger(evaled, 2, "2", true);
}

TEST_F(ValueFactoryTest, ListCdrTest)
{
    ScamValue one = makeInteger(1, true);
    ScamValue two = makeInteger(2, true);
    ScamValue three = makeInteger(3, true);
    ScamValue list = makeList(one, two, three, two, one);
    expectList(list, "(1 2 3 2 1)", 5);

    ScamValue cdr0 = nthcdr(list, 0);
    expectList(cdr0, "(2 3 2 1)", 4);

    ScamValue cdr3 = nthcdr(list, 3);
    expectList(cdr3, "(1)", 1);

    ScamValue cdr4 = nthcdr(list, 4);
    expectNull(cdr4);

    ScamValue cdr5 = nthcdr(list, 5);
    expectError(cdr5);
}

TEST_F(ValueFactoryTest, PseudoListCdrTest)
{
    ScamValue one   = makeInteger(1, true);
    ScamValue two   = makeInteger(2, true);
    ScamValue three = makeInteger(3, true);

    ScamValue pair  = makePair(two, three);
    ScamValue plist = makePair(one, pair);

    expectPair(plist, "(1 2 . 3)");

    ScamValue cdr0 = nthcdr(plist, 0);
    expectPair(cdr0, "(2 . 3)");

    ScamValue cdr1 = nthcdr(plist, 1);
    expectInteger(cdr1, 3, "3", true);

    ScamValue cdr2 = nthcdr(plist, 2);
    expectError(cdr2);
}

TEST_F(ValueFactoryTest, SpecialFormQuote)
{
    string const value { "Special Form quote" };

    ScamValue quote  = makeSpecialForm("quote", applyQuote, nullptr, true);
    expectApplicable(quote, value, true);

    ScamValue evaled = evaluate(quote);
    expectApplicable(evaled, value, true);
}

TEST_F(ValueFactoryTest, SpecialFormQuasiQuote)
{
    string const value { "Special Form quasiquote" };

    ScamValue quote  = makeSpecialForm("quasiquote", applyQuasiQuote, nullptr, true);
    expectApplicable(quote, value, true);

    ScamValue evaled = evaluate(quote);
    expectApplicable(evaled, value, true);
}

TEST_F(ValueFactoryTest, VectorEmpty)
{
    string const value { "#()" };
    ExprVec vec;
    ScamValue expr  = makeVector(vec);
    expectVector(expr, value, 0);

    ScamValue evaled = evaluate(expr);
    expectVector(evaled, value, 0);
}

TEST_F(ValueFactoryTest, VectorNonEmpty)
{
    string const value { "#(1 \"2\" 3)" };
    ExprVec vec;
    vec.push_back(makeInteger(1, true));
    vec.push_back(makeString("2"));
    vec.push_back(makeInteger(3, true));

    auto f = [this, &value] (ScamValue expr) {
                 expectVector(expr, value, 3u);
                 expectInteger(nthcar(expr, 0), 1, "1", true);
                 expectString(nthcar(expr, 1), "\"2\"");
                 expectInteger(nthcar(expr, 2), 3, "3", true);
                 expectError(nthcar(expr, 3));
             };

    ScamValue expr  = makeVector(vec);
    f(expr);

    ScamValue evaled = evaluate(expr);
    f(evaled);
}

TEST_F(ValueFactoryTest, ByteVectorEmpty)
{
    string const value { "#u8()" };
    ByteVec vec;
    ScamValue expr  = makeByteVector(vec);
    expectByteVector(expr, value, 0);

    ScamValue evaled = evaluate(expr);
    expectByteVector(evaled, value, 0);
}

TEST_F(ValueFactoryTest, ByteVectorNonEmpty)
{
    string const value { "#u8(1 2 3)" };
    ByteVec vec { 1, 2, 3 };

    auto f = [this, &value] (ScamValue expr) {
                 expectByteVector(expr, value, 3u);
                 expectInteger(nthcar(expr, 0), 1, "1", true);
                 expectInteger(nthcar(expr, 1), 2, "2", true);
                 expectInteger(nthcar(expr, 2), 3, "3", true);
                 expectError(nthcar(expr, 3));
             };

    ScamValue expr  = makeByteVector(vec);
    f(expr);

    ScamValue evaled = evaluate(expr);
    f(evaled);
}

TEST_F(ValueFactoryTest, DictNewEmpty)
{
    ScamValue expr = makeDict();
    expectDict(expr, 0u, "{}");
}

TEST_F(ValueFactoryTest, DictNewSingleton)
{
    ExprVec vec;
    vec.push_back(makeInteger(1, true));
    vec.push_back(makeString("one"));

    ScamValue expr = makeDict(vec);
    expectDict(expr, 1u, "{ 1 \"one\" }");
}

TEST_F(ValueFactoryTest, DictNewSingletonDupKeys)
{
    ExprVec vec;
    vec.push_back(makeInteger(1, true));
    vec.push_back(makeString("one"));
    vec.push_back(makeInteger(1, true));
    vec.push_back(makeString("ein"));

    ScamValue expr = makeDict(vec);
    expectDict(expr, 1u, "{ 1 \"ein\" }");
}

TEST_F(ValueFactoryTest, SpecialNumericNaN)
{
    ScamValue expr = makeNaN();
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(ValueFactoryTest, SpecialNumericNegInf)
{
    ScamValue expr = makeNegInf();
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(ValueFactoryTest, SpecialNumericPosInf)
{
    ScamValue expr = makePosInf();
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(ValueFactoryTest, EnvTest)
{
    ScamValue rv = makeNothing();
    try {
        Env * env = mm.make<Env>();
        ScamValue key = makeSymbol("x");

        ScamValue test = env->put(key, makeInteger(2, true));
        ASSERT_TRUE(isNothing(test));

        ScamValue expr = makeEnv(env);
        expectEnv(expr);

        Env * env2 = asEnv(expr);
        EXPECT_EQ(env, env2);
        expectInteger(env2->get(key), 2, "2", true);

        rv = makeSymbol("ok");
    }
    catch ( ScamException e ) {
        rv = makeError(e.getMessage().c_str());
    }
    catch ( std::exception & e ) {
        rv = makeError(e.what());
    }
    catch ( ... ) {
        rv = makeError("Unknown exception");
    }

    cerr << writeValue(rv) << "\n";
}
