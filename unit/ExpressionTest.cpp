#include "TestBase.hpp"

#include "ScamException.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AllSpecialForms.hpp"

using namespace std;
using namespace scam;

class ExpressionTest : public TestBase
{
protected:
    ExpressionTest()
        : TestBase(false)
    {
    }
};

TEST_F(ExpressionTest, NullExpression)
{
    ScamValue expr = makeNull();
    expectNull(expr);

    ScamValue evaled = evaluate(expr);
    expectError(evaled, "The null type cannot be evaluated.", false);
}

TEST_F(ExpressionTest, ErrorExpression)
{
    string const msg("Test message");

    ScamValue expr = makeError(msg);
    expectError(expr, msg);

    ScamValue evaled = evaluate(expr);
    expectError(evaled, msg);
}

TEST_F(ExpressionTest, BooleanTrue)
{
    ScamValue expr = makeBoolean(true);
    booleanTest(expr, true, "#t");
}

TEST_F(ExpressionTest, BooleanFalse)
{
    ScamValue expr = makeBoolean(false);
    booleanTest(expr, false, "#f");
}

TEST_F(ExpressionTest, ComplexTest)
{
    string const repr{ "6-i" };

    ScamValue real = makeReal(6, true);
    ScamValue imag = makeReal(-1, true);
    ScamValue expr = makeComplex(real, imag);
    expectComplex(expr, real, imag, repr, true);

    ScamValue evaled = evaluate(expr);
    expectComplex(evaled, real, imag, repr, true);
}

TEST_F(ExpressionTest, RealTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    ScamValue expr = makeReal(value, false);
    expectReal(expr, value, repr, false);

    ScamValue evaled = evaluate(expr);
    expectReal(evaled, value, repr, false);
}

TEST_F(ExpressionTest, RationalTest)
{
    RationalPair value { 8, 3 };
    string const repr{ "8/3" };

    ScamValue expr = makeRational(8, 3, true);
    expectRational(expr, value, repr, true);

    ScamValue evaled = evaluate(expr);
    expectRational(evaled, value, repr, true);
}

TEST_F(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    ScamValue expr = makeInteger(value, true);
    expectInteger(expr, value, repr, true);

    ScamValue evaled = evaluate(expr);
    expectInteger(evaled, value, repr, true);
}

TEST_F(ExpressionTest, CharacterTest)
{
    string const repr { "#\\Q" };
    char value { 'Q' };

    ScamValue expr = makeCharacter(repr);
    expectChar(expr, value, repr);

    ScamValue evaled = evaluate(expr);
    expectChar(evaled, value, repr);
}

TEST_F(ExpressionTest, StringTest)
{
    string const value { "Fnord!" };

    ScamValue expr = makeString(value);
    expectString(expr, value);

    ScamValue evaled = evaluate(expr);
    expectString(evaled, value);
}

TEST_F(ExpressionTest, SymbolTest)
{
    string const name { "Fnord!" };

    ScamValue sym = makeSymbol(name);
    expectSymbol(sym, name);

    ScamValue evaled = evaluate(sym);
    expectError(evaled);

    ScamValue value = makeInteger(1899, true);
    engine.addBinding(sym, value);
    evaled = evaluate(sym);
    expectInteger(evaled, 1899, "1899", true);
}

TEST_F(ExpressionTest, NilTest)
{
    ScamValue expr = makeNil();
    expectNil(expr);

    ScamValue evaled = evaluate(expr);
    expectNil(evaled);
}

TEST_F(ExpressionTest, ConsSingletonTest)
{
    string const value { "(works)" };

    ScamValue car = makeSymbol("works");
    ScamValue cdr = makeNil();
    ScamValue expr = makeCons(car, cdr);

    expectList(expr, value, 1);

    ScamValue first = nthcar(expr, 0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ConsDoubletonTest)
{
    string const value { "(works also)" };

    ScamValue car  = makeSymbol("works");
    ScamValue cadr = makeSymbol("also");
    ScamValue cddr = makeNil();
    ScamValue cdr  = makeCons(cadr, cddr);;
    ScamValue expr = makeCons(car, cdr);

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
    expectNil(cdr3);
}

TEST_F(ExpressionTest, ListEmptyTest)
{
    ScamValue expr = makeList();
    expectNil(expr);

    ScamValue evaled = evaluate(expr);
    expectNil(evaled);
}

TEST_F(ExpressionTest, ListSingletonTest)
{
    string const value { "(works)" };

    ScamValue car = makeSymbol("works");
    ScamValue expr = makeList(car);

    expectList(expr, value, 1);

    ScamValue first = nthcar(expr, 0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ListDoubletonTest)
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

TEST_F(ExpressionTest, ConsDottedPair)
{
    string const value { "(1 . 2)" };

    ScamValue car = makeInteger(1, true);
    ScamValue cdr = makeInteger(2, true);
    ScamValue expr = makeCons(car, cdr);

    expectCons(expr, value);

    expectInteger(getCar(expr), 1, "1", true);
    expectInteger(getCdr(expr), 2, "2", true);
}

TEST_F(ExpressionTest, ConsEvalTest)
{
    string const value { "(quote 2)" };

    ScamValue car  = makeSymbol("quote");
    ScamValue cadr = makeInteger(2, true);
    ScamValue cddr = makeNil();
    ScamValue cdr  = makeCons(cadr, cddr);;
    ScamValue expr = makeCons(car, cdr);

    expectList(expr, value, 2);
    expectSymbol(getCar(expr), "quote");

    ScamValue evaled = evaluate(expr);
    expectInteger(evaled, 2, "2", true);
}

TEST_F(ExpressionTest, ListCdrTest)
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
    expectNil(cdr4);

    ScamValue cdr5 = nthcdr(list, 5);
    expectError(cdr5);
}

TEST_F(ExpressionTest, PseudoListCdrTest)
{
    ScamValue one   = makeInteger(1, true);
    ScamValue two   = makeInteger(2, true);
    ScamValue three = makeInteger(3, true);

    ScamValue cons  = makeCons(two, three);
    ScamValue plist = makeCons(one, cons);

    expectCons(plist, "(1 2 . 3)");

    ScamValue cdr0 = nthcdr(plist, 0);
    expectCons(cdr0, "(2 . 3)");

    ScamValue cdr1 = nthcdr(plist, 1);
    expectInteger(cdr1, 3, "3", true);

    ScamValue cdr2 = nthcdr(plist, 2);
    expectError(cdr2);
}

TEST_F(ExpressionTest, SpecialFormQuote)
{
    string const value { "Special Form quote" };

    ScamValue quote  = makeSpecialForm("quote", applyQuote, nullptr, true);
    expectApplicable(quote, value, true);

    ScamValue evaled = evaluate(quote);
    expectApplicable(evaled, value, true);
}

TEST_F(ExpressionTest, SpecialFormQuasiQuote)
{
    string const value { "Special Form quasiquote" };

    ScamValue quote  = makeSpecialForm("quasiquote", applyQuasiQuote, nullptr, true);
    expectApplicable(quote, value, true);

    ScamValue evaled = evaluate(quote);
    expectApplicable(evaled, value, true);
}

TEST_F(ExpressionTest, VectorEmpty)
{
    string const value { "#()" };
    ExprVec vec;
    ScamValue expr  = makeVector(vec);
    expectVector(expr, value, 0);

    ScamValue evaled = evaluate(expr);
    expectVector(evaled, value, 0);
}

TEST_F(ExpressionTest, VectorNonEmpty)
{
    string const value { "#(1 2 3)" };
    ExprVec vec;
    vec.push_back(makeInteger(1, true));
    vec.push_back(makeString("2"));
    vec.push_back(makeInteger(3, true));

    auto f = [this, &value] (ScamValue expr) {
                 expectVector(expr, value, 3u);
                 expectInteger(nthcar(expr, 0), 1, "1", true);
                 expectString(nthcar(expr, 1), "2");
                 expectInteger(nthcar(expr, 2), 3, "3", true);
                 expectError(nthcar(expr, 3));
             };

    ScamValue expr  = makeVector(vec);
    f(expr);

    ScamValue evaled = evaluate(expr);
    f(evaled);
}

TEST_F(ExpressionTest, ByteVectorEmpty)
{
    string const value { "#u8()" };
    ByteVec vec;
    ScamValue expr  = makeByteVector(vec);
    expectByteVector(expr, value, 0);

    ScamValue evaled = evaluate(expr);
    expectByteVector(evaled, value, 0);
}

TEST_F(ExpressionTest, ByteVectorNonEmpty)
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

TEST_F(ExpressionTest, DictNewEmpty)
{
    ScamValue expr = makeDict();
    expectDict(expr, 0u, "{}");
}

TEST_F(ExpressionTest, DictNewSingleton)
{
    ExprVec vec;
    vec.push_back(makeInteger(1, true));
    vec.push_back(makeString("one"));

    ScamValue expr = makeDict(vec);
    expectDict(expr, 1u, "{ 1 one }");
}

TEST_F(ExpressionTest, DictNewSingletonDupKeys)
{
    ExprVec vec;
    vec.push_back(makeInteger(1, true));
    vec.push_back(makeString("one"));
    vec.push_back(makeInteger(1, true));
    vec.push_back(makeString("ein"));

    ScamValue expr = makeDict(vec);
    expectDict(expr, 1u, "{ 1 ein }");
}

TEST_F(ExpressionTest, SpecialNumericNaN)
{
    ScamValue expr = makeNaN();
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(ExpressionTest, SpecialNumericNegInf)
{
    ScamValue expr = makeNegInf();
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(ExpressionTest, SpecialNumericPosInf)
{
    ScamValue expr = makePosInf();
    expectSpecialNumeric(expr, "+inf.0");
}
