#include "TestBase.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/Quote.hpp"
#include "form/QuasiQuote.hpp"

using namespace std;
using namespace scam;

class ExpressionTest : public TestBase
{
};

TEST_F(ExpressionTest, NullExpression)
{
    ExprHandle expr = ExpressionFactory::makeNull();
    expectNull(expr);

    ExprHandle evaled = evaluate(expr);
    expectError(evaled, "The null type cannot be evaluated.", false);
}

TEST_F(ExpressionTest, ErrorExpression)
{
    string const msg("Test message");

    ExprHandle expr = ExpressionFactory::makeError(msg);
    expectError(expr, msg);

    ExprHandle evaled = evaluate(expr);
    expectError(evaled, msg);
}

TEST_F(ExpressionTest, BooleanTrue)
{
    ExprHandle expr = ExpressionFactory::makeBoolean(true);
    booleanTest(expr, true, "#t");
}

TEST_F(ExpressionTest, BooleanFalse)
{
    ExprHandle expr = ExpressionFactory::makeBoolean(false);
    booleanTest(expr, false, "#f");
}

TEST_F(ExpressionTest, RealTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    ExprHandle expr = ExpressionFactory::makeReal(value, false);
    expectReal(expr, value, repr, false);

    ExprHandle evaled = evaluate(expr);
    expectReal(evaled, value, repr, false);
}

TEST_F(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    ExprHandle expr = ExpressionFactory::makeInteger(value, true);
    expectInteger(expr, value, repr, true);

    ExprHandle evaled = evaluate(expr);
    expectInteger(evaled, value, repr, true);
}

TEST_F(ExpressionTest, CharacterTest)
{
    string const repr { "#\\Q" };
    char value { 'Q' };

    ExprHandle expr = ExpressionFactory::makeCharacter(repr);
    expectChar(expr, value, repr);

    ExprHandle evaled = evaluate(expr);
    expectChar(evaled, value, repr);
}

TEST_F(ExpressionTest, StringTest)
{
    string const value { "Fnord!" };

    ExprHandle expr = ExpressionFactory::makeString(value);
    expectString(expr, value);

    ExprHandle evaled = evaluate(expr);
    expectString(evaled, value);
}

TEST_F(ExpressionTest, SymbolTest)
{
    string const name { "Fnord!" };

    ExprHandle sym = ExpressionFactory::makeSymbol(name);
    expectSymbol(sym, name);

    ExprHandle evaled = evaluate(sym);
    expectError(evaled);

    ExprHandle value = ExpressionFactory::makeInteger(1899, true);
    engine.addBinding(dynamic_cast<ScamSymbol*>(sym), value);
    evaled = evaluate(sym);
    expectInteger(evaled, 1899, "1899", true);
}

TEST_F(ExpressionTest, NilTest)
{
    ExprHandle expr = ExpressionFactory::makeNil();
    expectNil(expr);

    ExprHandle evaled = evaluate(expr);
    expectNil(evaled);
}

TEST_F(ExpressionTest, ConsSingletonTest)
{
    string const value { "(works)" };

    ExprHandle car = ExpressionFactory::makeSymbol("works");
    ExprHandle cdr = ExpressionFactory::makeNil();
    ExprHandle expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 1);

    ExprHandle first = expr->nthcar(0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ConsDoubletonTest)
{
    string const value { "(works also)" };

    ExprHandle car  = ExpressionFactory::makeSymbol("works");
    ExprHandle cadr = ExpressionFactory::makeSymbol("also");
    ExprHandle cddr = ExpressionFactory::makeNil();
    ExprHandle cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    ExprHandle expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 2);

    ExprHandle first = expr->nthcar(0);
    expectSymbol(first, "works");

    ExprHandle second = expr->nthcar(1);
    expectSymbol(second, "also");

    ExprHandle third = expr->nthcar(2);
    expectError(third);

    ExprHandle car2 = expr->getCar();
    expectSymbol(car2, "works");

    ExprHandle cdr2 = expr->getCdr();
    expectList(cdr2, "(also)", 1);

    ExprHandle cdr3 = expr->nthcdr(1);
    expectNil(cdr3);
}

TEST_F(ExpressionTest, ListEmptyTest)
{
    ExprHandle expr = ExpressionFactory::makeList();
    expectNil(expr);

    ExprHandle evaled = evaluate(expr);
    expectNil(evaled);
}

TEST_F(ExpressionTest, ListSingletonTest)
{
    string const value { "(works)" };

    ExprHandle car = ExpressionFactory::makeSymbol("works");
    ExprHandle expr = ExpressionFactory::makeList(car);

    expectList(expr, value, 1);

    ExprHandle first = expr->nthcar(0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ListDoubletonTest)
{
    string const value { "(works also)" };

    ExprHandle car0  = ExpressionFactory::makeSymbol("works");
    ExprHandle car1 = ExpressionFactory::makeSymbol("also");
    ExprHandle expr = ExpressionFactory::makeList(car0, car1);

    expectList(expr, value, 2);

    ExprHandle first = expr->nthcar(0);
    expectSymbol(first, "works");

    ExprHandle second = expr->nthcar(1);
    expectSymbol(second, "also");

    ExprHandle third = expr->nthcar(2);
    expectError(third);

    ExprHandle car2 = expr->getCar();
    expectSymbol(car2, "works");

    ExprHandle cdr2 = expr->getCdr();
    expectList(cdr2, "(also)", 1);
}

TEST_F(ExpressionTest, ConsDottedPair)
{
    string const value { "(1 . 2)" };

    ExprHandle car = ExpressionFactory::makeInteger(1, true);
    ExprHandle cdr = ExpressionFactory::makeInteger(2, true);
    ExprHandle expr = ExpressionFactory::makeCons(car, cdr);

    expectCons(expr, value);

    expectInteger(expr->getCar(), 1, "1", true);
    expectInteger(expr->getCdr(), 2, "2", true);
}

TEST_F(ExpressionTest, ConsEvalTest)
{
    string const value { "(quote 2)" };

    ExprHandle car  = ExpressionFactory::makeSymbol("quote");
    ExprHandle cadr = ExpressionFactory::makeInteger(2, true);
    ExprHandle cddr = ExpressionFactory::makeNil();
    ExprHandle cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    ExprHandle expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 2);
    expectSymbol(expr->getCar(), "quote");

    ExprHandle evaled = evaluate(expr);
    expectInteger(evaled, 2, "2", true);
}

TEST_F(ExpressionTest, ListCdrTest)
{
    ExprHandle one = ExpressionFactory::makeInteger(1, true);
    ExprHandle two = ExpressionFactory::makeInteger(2, true);
    ExprHandle three = ExpressionFactory::makeInteger(3, true);

    ExprHandle list =
        ExpressionFactory::makeList(one, two, three, two, one);
    expectList(list, "(1 2 3 2 1)", 5);

    ExprHandle cdr0 = list->nthcdr(0);
    expectList(cdr0, "(2 3 2 1)", 4);

    ExprHandle cdr3 = list->nthcdr(3);
    expectList(cdr3, "(1)", 1);

    ExprHandle cdr4 = list->nthcdr(4);
    expectNil(cdr4);

    ExprHandle cdr5 = list->nthcdr(5);
    expectError(cdr5);
}

TEST_F(ExpressionTest, PseudoListCdrTest)
{
    ExprHandle one = ExpressionFactory::makeInteger(1, true);
    ExprHandle two = ExpressionFactory::makeInteger(2, true);
    ExprHandle three = ExpressionFactory::makeInteger(3, true);

    ExprHandle cons  = ExpressionFactory::makeCons(two, three);
    ExprHandle plist = ExpressionFactory::makeCons(one, cons);

    expectCons(plist, "(1 2 . 3)");

    ExprHandle cdr0 = plist->nthcdr(0);
    expectCons(cdr0, "(2 . 3)");

    ExprHandle cdr1 = plist->nthcdr(1);
    expectInteger(cdr1, 3, "3", true);

    ExprHandle cdr2 = plist->nthcdr(2);
    expectError(cdr2);
}

TEST_F(ExpressionTest, SpecialFormQuote)
{
    string const value { "Special Form quote" };

    ExprHandle quote  = ExpressionFactory::makeForm<Quote>();
    expectApplicable(quote, value);

    ExprHandle evaled = evaluate(quote);
    expectApplicable(evaled, value);
}

TEST_F(ExpressionTest, SpecialFormQuasiQuote)
{
    string const value { "Special Form quasiquote" };

    ExprHandle quote  = ExpressionFactory::makeForm<QuasiQuote>();
    expectApplicable(quote, value);

    ExprHandle evaled = evaluate(quote);
    expectApplicable(evaled, value);
}

TEST_F(ExpressionTest, VectorEmpty)
{
    string const value { "#()" };
    ExprVec vec;
    ExprHandle expr  = ExpressionFactory::makeVector(vec);
    expectVector(expr, value, 0);

    ExprHandle evaled = evaluate(expr);
    expectVector(evaled, value, 0);
}

TEST_F(ExpressionTest, VectorNonEmpty)
{
    string const value { "#(1 2 3)" };
    ExprVec vec;
    for ( auto i : { 1, 2, 3 } ) {
        vec.push_back(ExpressionFactory::makeInteger(i, true));
    }

    auto f = [this, &value] (ExprHandle expr) {
                 expectVector(expr, value, 3u);
                 expectInteger(expr->nthcar(0), 1, "1", true);
                 expectInteger(expr->nthcar(1), 2, "2", true);
                 expectInteger(expr->nthcar(2), 3, "3", true);
                 expectError(expr->nthcar(3));
             };

    ExprHandle expr  = ExpressionFactory::makeVector(vec);
    f(expr);

    ExprHandle evaled = evaluate(expr);
    f(evaled);
}

TEST_F(ExpressionTest, DictNewEmpty)
{
    ExprHandle expr = ExpressionFactory::makeDict();
    expectDict(expr, 0u, "{}");
}

TEST_F(ExpressionTest, DictNewSingleton)
{
    ExprVec vec;
    vec.push_back(ExpressionFactory::makeInteger(1, true));
    vec.push_back(ExpressionFactory::makeString("one"));

    ExprHandle expr = ExpressionFactory::makeDict(vec);
    expectDict(expr, 1u, "{ 1 one }");
}

TEST_F(ExpressionTest, DictNewSingletonDupKeys)
{
    ExprVec vec;
    vec.push_back(ExpressionFactory::makeInteger(1, true));
    vec.push_back(ExpressionFactory::makeString("one"));
    vec.push_back(ExpressionFactory::makeInteger(1, true));
    vec.push_back(ExpressionFactory::makeString("ein"));

    ExprHandle expr = ExpressionFactory::makeDict(vec);
    expectDict(expr, 1u, "{ 1 ein }");
}

TEST_F(ExpressionTest, SpecialNumericNaN)
{
    ExprHandle expr = ExpressionFactory::makeNaN();
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(ExpressionTest, SpecialNumericPosInf)
{
    ExprHandle expr = ExpressionFactory::makePosInf();
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(ExpressionTest, SpecialNumericNegInf)
{
    ExprHandle expr = ExpressionFactory::makeNegInf();
    expectSpecialNumeric(expr, "-inf.0");
}
