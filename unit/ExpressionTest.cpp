#include "TestBase.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/Quote.hpp"
#include "form/QuasiQuote.hpp"

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
    ScamValue expr = ExpressionFactory::makeNull();
    expectNull(expr);

    ScamValue evaled = evaluate(expr);
    expectError(evaled, "The null type cannot be evaluated.", false);
}

TEST_F(ExpressionTest, ErrorExpression)
{
    string const msg("Test message");

    ScamValue expr = ExpressionFactory::makeError(msg);
    expectError(expr, msg);

    ScamValue evaled = evaluate(expr);
    expectError(evaled, msg);
}

TEST_F(ExpressionTest, BooleanTrue)
{
    ScamValue expr = ExpressionFactory::makeBoolean(true);
    booleanTest(expr, true, "#t");
}

TEST_F(ExpressionTest, BooleanFalse)
{
    ScamValue expr = ExpressionFactory::makeBoolean(false);
    booleanTest(expr, false, "#f");
}

TEST_F(ExpressionTest, ComplexTest)
{
    string const repr{ "6-i" };

    ScamValue real = ExpressionFactory::makeReal(6, true);
    ScamValue imag = ExpressionFactory::makeReal(-1, true);
    ScamValue expr = ExpressionFactory::makeComplex(real, imag);
    expectComplex(expr, real, imag, repr, true);

    ScamValue evaled = evaluate(expr);
    expectComplex(evaled, real, imag, repr, true);
}

TEST_F(ExpressionTest, RealTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    ScamValue expr = ExpressionFactory::makeReal(value, false);
    expectReal(expr, value, repr, false);

    ScamValue evaled = evaluate(expr);
    expectReal(evaled, value, repr, false);
}

TEST_F(ExpressionTest, RationalTest)
{
    RationalPair value { 8, 3 };
    string const repr{ "8/3" };

    ScamValue expr = ExpressionFactory::makeRational(8, 3, true);
    expectRational(expr, value, repr, true);

    ScamValue evaled = evaluate(expr);
    expectRational(evaled, value, repr, true);
}

TEST_F(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    ScamValue expr = ExpressionFactory::makeInteger(value, true);
    expectInteger(expr, value, repr, true);

    ScamValue evaled = evaluate(expr);
    expectInteger(evaled, value, repr, true);
}

TEST_F(ExpressionTest, CharacterTest)
{
    string const repr { "#\\Q" };
    char value { 'Q' };

    ScamValue expr = ExpressionFactory::makeCharacter(repr);
    expectChar(expr, value, repr);

    ScamValue evaled = evaluate(expr);
    expectChar(evaled, value, repr);
}

TEST_F(ExpressionTest, StringTest)
{
    string const value { "Fnord!" };

    ScamValue expr = ExpressionFactory::makeString(value);
    expectString(expr, value);

    ScamValue evaled = evaluate(expr);
    expectString(evaled, value);
}

TEST_F(ExpressionTest, SymbolTest)
{
    string const name { "Fnord!" };

    ScamValue sym = ExpressionFactory::makeSymbol(name);
    expectSymbol(sym, name);

    ScamValue evaled = evaluate(sym);
    expectError(evaled);

    ScamValue value = ExpressionFactory::makeInteger(1899, true);
    engine.addBinding(dynamic_cast<ScamSymbol*>(sym), value);
    evaled = evaluate(sym);
    expectInteger(evaled, 1899, "1899", true);
}

TEST_F(ExpressionTest, NilTest)
{
    ScamValue expr = ExpressionFactory::makeNil();
    expectNil(expr);

    ScamValue evaled = evaluate(expr);
    expectNil(evaled);
}

TEST_F(ExpressionTest, ConsSingletonTest)
{
    string const value { "(works)" };

    ScamValue car = ExpressionFactory::makeSymbol("works");
    ScamValue cdr = ExpressionFactory::makeNil();
    ScamValue expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 1);

    ScamValue first = expr->nthcar(0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ConsDoubletonTest)
{
    string const value { "(works also)" };

    ScamValue car  = ExpressionFactory::makeSymbol("works");
    ScamValue cadr = ExpressionFactory::makeSymbol("also");
    ScamValue cddr = ExpressionFactory::makeNil();
    ScamValue cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    ScamValue expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 2);

    ScamValue first = expr->nthcar(0);
    expectSymbol(first, "works");

    ScamValue second = expr->nthcar(1);
    expectSymbol(second, "also");

    ScamValue third = expr->nthcar(2);
    expectError(third);

    ScamValue car2 = expr->getCar();
    expectSymbol(car2, "works");

    ScamValue cdr2 = expr->getCdr();
    expectList(cdr2, "(also)", 1);

    ScamValue cdr3 = expr->nthcdr(1);
    expectNil(cdr3);
}

TEST_F(ExpressionTest, ListEmptyTest)
{
    ScamValue expr = ExpressionFactory::makeList();
    expectNil(expr);

    ScamValue evaled = evaluate(expr);
    expectNil(evaled);
}

TEST_F(ExpressionTest, ListSingletonTest)
{
    string const value { "(works)" };

    ScamValue car = ExpressionFactory::makeSymbol("works");
    ScamValue expr = ExpressionFactory::makeList(car);

    expectList(expr, value, 1);

    ScamValue first = expr->nthcar(0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ListDoubletonTest)
{
    string const value { "(works also)" };

    ScamValue car0  = ExpressionFactory::makeSymbol("works");
    ScamValue car1 = ExpressionFactory::makeSymbol("also");
    ScamValue expr = ExpressionFactory::makeList(car0, car1);

    expectList(expr, value, 2);

    ScamValue first = expr->nthcar(0);
    expectSymbol(first, "works");

    ScamValue second = expr->nthcar(1);
    expectSymbol(second, "also");

    ScamValue third = expr->nthcar(2);
    expectError(third);

    ScamValue car2 = expr->getCar();
    expectSymbol(car2, "works");

    ScamValue cdr2 = expr->getCdr();
    expectList(cdr2, "(also)", 1);
}

TEST_F(ExpressionTest, ConsDottedPair)
{
    string const value { "(1 . 2)" };

    ScamValue car = ExpressionFactory::makeInteger(1, true);
    ScamValue cdr = ExpressionFactory::makeInteger(2, true);
    ScamValue expr = ExpressionFactory::makeCons(car, cdr);

    expectCons(expr, value);

    expectInteger(expr->getCar(), 1, "1", true);
    expectInteger(expr->getCdr(), 2, "2", true);
}

TEST_F(ExpressionTest, ConsEvalTest)
{
    string const value { "(quote 2)" };

    ScamValue car  = ExpressionFactory::makeSymbol("quote");
    ScamValue cadr = ExpressionFactory::makeInteger(2, true);
    ScamValue cddr = ExpressionFactory::makeNil();
    ScamValue cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    ScamValue expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 2);
    expectSymbol(expr->getCar(), "quote");

    ScamValue evaled = evaluate(expr);
    expectInteger(evaled, 2, "2", true);
}

TEST_F(ExpressionTest, ListCdrTest)
{
    ScamValue one = ExpressionFactory::makeInteger(1, true);
    ScamValue two = ExpressionFactory::makeInteger(2, true);
    ScamValue three = ExpressionFactory::makeInteger(3, true);

    ScamValue list =
        ExpressionFactory::makeList(one, two, three, two, one);
    expectList(list, "(1 2 3 2 1)", 5);

    ScamValue cdr0 = list->nthcdr(0);
    expectList(cdr0, "(2 3 2 1)", 4);

    ScamValue cdr3 = list->nthcdr(3);
    expectList(cdr3, "(1)", 1);

    ScamValue cdr4 = list->nthcdr(4);
    expectNil(cdr4);

    ScamValue cdr5 = list->nthcdr(5);
    expectError(cdr5);
}

TEST_F(ExpressionTest, PseudoListCdrTest)
{
    ScamValue one = ExpressionFactory::makeInteger(1, true);
    ScamValue two = ExpressionFactory::makeInteger(2, true);
    ScamValue three = ExpressionFactory::makeInteger(3, true);

    ScamValue cons  = ExpressionFactory::makeCons(two, three);
    ScamValue plist = ExpressionFactory::makeCons(one, cons);

    expectCons(plist, "(1 2 . 3)");

    ScamValue cdr0 = plist->nthcdr(0);
    expectCons(cdr0, "(2 . 3)");

    ScamValue cdr1 = plist->nthcdr(1);
    expectInteger(cdr1, 3, "3", true);

    ScamValue cdr2 = plist->nthcdr(2);
    expectError(cdr2);
}

TEST_F(ExpressionTest, SpecialFormQuote)
{
    string const value { "Special Form quote" };

    ScamValue quote  = ExpressionFactory::makeForm<Quote>();
    expectApplicable(quote, value);

    ScamValue evaled = evaluate(quote);
    expectApplicable(evaled, value);
}

TEST_F(ExpressionTest, SpecialFormQuasiQuote)
{
    string const value { "Special Form quasiquote" };

    ScamValue quote  = ExpressionFactory::makeForm<QuasiQuote>();
    expectApplicable(quote, value);

    ScamValue evaled = evaluate(quote);
    expectApplicable(evaled, value);
}

TEST_F(ExpressionTest, VectorEmpty)
{
    string const value { "#()" };
    ExprVec vec;
    ScamValue expr  = ExpressionFactory::makeVector(vec);
    expectVector(expr, value, 0);

    ScamValue evaled = evaluate(expr);
    expectVector(evaled, value, 0);
}

TEST_F(ExpressionTest, VectorNonEmpty)
{
    string const value { "#(1 2 3)" };
    ExprVec vec;
    vec.push_back(ExpressionFactory::makeInteger(1, true));
    vec.push_back(ExpressionFactory::makeString("2"));
    vec.push_back(ExpressionFactory::makeInteger(3, true));

    auto f = [this, &value] (ScamValue expr) {
                 expectVector(expr, value, 3u);
                 expectInteger(expr->nthcar(0), 1, "1", true);
                 expectString(expr->nthcar(1), "2");
                 expectInteger(expr->nthcar(2), 3, "3", true);
                 expectError(expr->nthcar(3));
             };

    ScamValue expr  = ExpressionFactory::makeVector(vec);
    f(expr);

    ScamValue evaled = evaluate(expr);
    f(evaled);
}

TEST_F(ExpressionTest, ByteVectorEmpty)
{
    string const value { "#u8()" };
    ByteVec vec;
    ScamValue expr  = ExpressionFactory::makeByteVector(vec);
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
                 expectInteger(expr->nthcar(0), 1, "1", true);
                 expectInteger(expr->nthcar(1), 2, "2", true);
                 expectInteger(expr->nthcar(2), 3, "3", true);
                 expectError(expr->nthcar(3));
             };

    ScamValue expr  = ExpressionFactory::makeByteVector(vec);
    f(expr);

    ScamValue evaled = evaluate(expr);
    f(evaled);
}

TEST_F(ExpressionTest, DictNewEmpty)
{
    ScamValue expr = ExpressionFactory::makeDict();
    expectDict(expr, 0u, "{}");
}

TEST_F(ExpressionTest, DictNewSingleton)
{
    ExprVec vec;
    vec.push_back(ExpressionFactory::makeInteger(1, true));
    vec.push_back(ExpressionFactory::makeString("one"));

    ScamValue expr = ExpressionFactory::makeDict(vec);
    expectDict(expr, 1u, "{ 1 one }");
}

TEST_F(ExpressionTest, DictNewSingletonDupKeys)
{
    ExprVec vec;
    vec.push_back(ExpressionFactory::makeInteger(1, true));
    vec.push_back(ExpressionFactory::makeString("one"));
    vec.push_back(ExpressionFactory::makeInteger(1, true));
    vec.push_back(ExpressionFactory::makeString("ein"));

    ScamValue expr = ExpressionFactory::makeDict(vec);
    expectDict(expr, 1u, "{ 1 ein }");
}

TEST_F(ExpressionTest, SpecialNumericNaN)
{
    ScamValue expr = ExpressionFactory::makeNaN();
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(ExpressionTest, SpecialNumericPosInf)
{
    ScamValue expr = ExpressionFactory::makePosInf();
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(ExpressionTest, SpecialNumericNegInf)
{
    ScamValue expr = ExpressionFactory::makeNegInf();
    expectSpecialNumeric(expr, "-inf.0");
}
