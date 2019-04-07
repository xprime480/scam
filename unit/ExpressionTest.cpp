
#include "ExpressionTestBase.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/Quote.hpp"

using namespace std;
using namespace scam;

class ExpressionTest : public ExpressionTestBase
{
};

TEST_F(ExpressionTest, NullExpression)
{
    ScamExpr * expr = ExpressionFactory::makeNull();
    expectNull(expr);

    ScamExpr * evaled = evaluate(expr);
    expectError(evaled);
}

TEST_F(ExpressionTest, ErrorExpression)
{
    string const msg("Test message");

    ScamExpr * expr = ExpressionFactory::makeError(msg);
    expectError(expr, msg);

    ScamExpr * evaled = evaluate(expr);
    expectError(evaled, msg);
}

TEST_F(ExpressionTest, BooleanTrue)
{
    ScamExpr * expr = ExpressionFactory::makeBoolean(true);
    booleanTest(expr, true, "#t");
}

TEST_F(ExpressionTest, BooleanFalse)
{
    ScamExpr * expr = ExpressionFactory::makeBoolean(false);
    booleanTest(expr, false, "#f");
}

TEST_F(ExpressionTest, FloatTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    ScamExpr * expr = ExpressionFactory::makeFloat(value);
    expectFloat(expr, value, repr);

    ScamExpr * evaled = evaluate(expr);
    expectFloat(evaled, value, repr);
}

TEST_F(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    ScamExpr * expr = ExpressionFactory::makeInteger(value);
    expectInteger(expr, value, repr);

    ScamExpr * evaled = evaluate(expr);
    expectInteger(evaled, value, repr);
}

TEST_F(ExpressionTest, CharacterTest)
{
    string const repr { "\\#Q" };
    char value { 'Q' };

    ScamExpr * expr = ExpressionFactory::makeCharacter(repr);
    expectChar(expr, value, repr);

    ScamExpr * evaled = evaluate(expr);
    expectChar(evaled, value, repr);
}

TEST_F(ExpressionTest, StringTest)
{
    string const value { "Fnord!" };

    ScamExpr * expr = ExpressionFactory::makeString(value);
    expectString(expr, value);

    ScamExpr * evaled = evaluate(expr);
    expectString(evaled, value);
}

TEST_F(ExpressionTest, SymbolTest)
{
    string const name { "Fnord!" };

    ScamExpr * sym = ExpressionFactory::makeSymbol(name);
    expectSymbol(sym, name);

    ScamExpr * evaled = evaluate(sym);
    expectError(evaled);

    ScamExpr * value = ExpressionFactory::makeInteger(1899);
    engine.addBinding(sym, value);
    evaled = evaluate(sym);
    expectInteger(evaled, 1899, "1899");
}

TEST_F(ExpressionTest, NilTest)
{
    ScamExpr * expr = ExpressionFactory::makeNil();
    expectNil(expr);

    ScamExpr * evaled = evaluate(expr);
    expectNil(evaled);
}

TEST_F(ExpressionTest, ConsSingletonTest)
{
    string const value { "(works)" };

    ScamExpr * car = ExpressionFactory::makeSymbol("works");
    ScamExpr * cdr = ExpressionFactory::makeNil();
    ScamExpr * expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 1);

    ScamExpr * first = expr->nthcar(0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ConsDoubletonTest)
{
    string const value { "(works also)" };

    ScamExpr * car  = ExpressionFactory::makeSymbol("works");
    ScamExpr * cadr = ExpressionFactory::makeSymbol("also");
    ScamExpr * cddr = ExpressionFactory::makeNil();
    ScamExpr * cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    ScamExpr * expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 2);

    ScamExpr * first = expr->nthcar(0);
    expectSymbol(first, "works");

    ScamExpr * second = expr->nthcar(1);
    expectSymbol(second, "also");

    ScamExpr * third = expr->nthcar(2);
    expectError(third);

    ScamExpr * car2 = expr->getCar();
    expectSymbol(car2, "works");

    ScamExpr * cdr2 = expr->getCdr();
    expectList(cdr2, "(also)", 1);

    ScamExpr * cdr3 = expr->nthcdr(1);
    expectNil(cdr3);
}

TEST_F(ExpressionTest, ListEmptyTest)
{
    ScamExpr * expr = ExpressionFactory::makeList();
    expectNil(expr);

    ScamExpr * evaled = evaluate(expr);
    expectNil(evaled);
}

TEST_F(ExpressionTest, ListSingletonTest)
{
    string const value { "(works)" };

    ScamExpr * car = ExpressionFactory::makeSymbol("works");
    ScamExpr * expr = ExpressionFactory::makeList(car);

    expectList(expr, value, 1);

    ScamExpr * first = expr->nthcar(0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ListDoubletonTest)
{
    string const value { "(works also)" };

    ScamExpr * car0  = ExpressionFactory::makeSymbol("works");
    ScamExpr * car1 = ExpressionFactory::makeSymbol("also");
    ScamExpr * expr = ExpressionFactory::makeList(car0, car1);

    expectList(expr, value, 2);

    ScamExpr * first = expr->nthcar(0);
    expectSymbol(first, "works");

    ScamExpr * second = expr->nthcar(1);
    expectSymbol(second, "also");

    ScamExpr * third = expr->nthcar(2);
    expectError(third);

    ScamExpr * car2 = expr->getCar();
    expectSymbol(car2, "works");

    ScamExpr * cdr2 = expr->getCdr();
    expectList(cdr2, "(also)", 1);
}

TEST_F(ExpressionTest, ConsDottedPair)
{
    string const value { "(1 . 2)" };

    ScamExpr * car = ExpressionFactory::makeInteger(1);
    ScamExpr * cdr = ExpressionFactory::makeInteger(2);
    ScamExpr * expr = ExpressionFactory::makeCons(car, cdr);

    expectCons(expr, value);

    expectInteger(expr->getCar(), 1, "1");
    expectInteger(expr->getCdr(), 2, "2");
}

TEST_F(ExpressionTest, ConsEvalTest)
{
    string const value { "(quote 2)" };

    ScamExpr * car  = ExpressionFactory::makeSymbol("quote");
    ScamExpr * cadr = ExpressionFactory::makeInteger(2);
    ScamExpr * cddr = ExpressionFactory::makeNil();
    ScamExpr * cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    ScamExpr * expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 2);
    expectSymbol(expr->getCar(), "quote");

    ScamExpr * evaled = evaluate(expr);
    expectInteger(evaled, 2, "2");
}

TEST_F(ExpressionTest, ListCdrTest)
{
    ScamExpr * one = ExpressionFactory::makeInteger(1);
    ScamExpr * two = ExpressionFactory::makeInteger(2);
    ScamExpr * three = ExpressionFactory::makeInteger(3);

    ScamExpr * list =
      ExpressionFactory::makeList(one, two, three, two, one);
    expectList(list, "(1 2 3 2 1)", 5);

    ScamExpr * cdr0 = list->nthcdr(0);
    expectList(cdr0, "(2 3 2 1)", 4);

    ScamExpr * cdr3 = list->nthcdr(3);
    expectList(cdr3, "(1)", 1);

    ScamExpr * cdr4 = list->nthcdr(4);
    expectNil(cdr4);

    ScamExpr * cdr5 = list->nthcdr(5);
    expectError(cdr5);
}

TEST_F(ExpressionTest, PseudoListCdrTest)
{
    ScamExpr * one = ExpressionFactory::makeInteger(1);
    ScamExpr * two = ExpressionFactory::makeInteger(2);
    ScamExpr * three = ExpressionFactory::makeInteger(3);

    ScamExpr * cons  = ExpressionFactory::makeCons(two, three);
    ScamExpr * plist = ExpressionFactory::makeCons(one, cons);

    expectCons(plist, "(1 2 . 3)");

    ScamExpr * cdr0 = plist->nthcdr(0);
    expectCons(cdr0, "(2 . 3)");

    ScamExpr * cdr1 = plist->nthcdr(1);
    expectInteger(cdr1, 3, "3");

    ScamExpr * cdr2 = plist->nthcdr(2);
    expectError(cdr2);
}

TEST_F(ExpressionTest, SpecialFormQuote)
{
    string const value { "Special Form quote" };

    ScamExpr * quote  = ExpressionFactory::makeForm<Quote>();
    expectApplicable(quote, value, false);

    ScamExpr * evaled = evaluate(quote);
    expectApplicable(evaled, value, false);
}

TEST_F(ExpressionTest, SpecialFormQuasiQuote)
{
    string const value { "Special Form quasiquote" };

    ScamExpr * quote  = ExpressionFactory::makeForm<QuasiQuote>();
    expectApplicable(quote, value, false);

    ScamExpr * evaled = evaluate(quote);
    expectApplicable(evaled, value, false);
}

TEST_F(ExpressionTest, VectorEmpty)
{
    string const value { "[]" };
    ExprVec vec;
    ScamExpr * expr  = ExpressionFactory::makeVector(vec);
    expectVector(expr, value, 0);

    ScamExpr * evaled = evaluate(expr);
    expectVector(evaled, value, 0);
}

TEST_F(ExpressionTest, VectorNonEmpty)
{
    string const value { "[1 2 3]" };
    ExprVec vec;
    for ( auto i : { 1, 2, 3 } ) {
        vec.push_back(ExpressionFactory::makeInteger(i));
    }

    auto f = [this, &value](ScamExpr * expr) {
        expectVector(expr, value, 3u);
        expectInteger(expr->nthcar(0), 1, "1");
        expectInteger(expr->nthcar(1), 2, "2");
        expectInteger(expr->nthcar(2), 3, "3");
        expectError(expr->nthcar(3));
    };

    ScamExpr * expr  = ExpressionFactory::makeVector(vec);
    f(expr);

    ScamExpr * evaled = evaluate(expr);
    f(evaled);
}

TEST_F(ExpressionTest, DictNewEmpty)
{
    ScamExpr * expr = ExpressionFactory::makeDict();
    expectDict(expr, 0u, "{}");
}

TEST_F(ExpressionTest, DictNewSingleton)
{
    ExprVec vec;
    vec.push_back(ExpressionFactory::makeInteger(1));
    vec.push_back(ExpressionFactory::makeString("one"));

    ScamExpr * expr = ExpressionFactory::makeDict(vec);
    expectDict(expr, 1u, "{ 1 one }");
}

TEST_F(ExpressionTest, DictNewSingletonDupKeys)
{
    ExprVec vec;
    vec.push_back(ExpressionFactory::makeInteger(1));
    vec.push_back(ExpressionFactory::makeString("one"));
    vec.push_back(ExpressionFactory::makeInteger(1));
    vec.push_back(ExpressionFactory::makeString("ein"));

    ScamExpr * expr = ExpressionFactory::makeDict(vec);
    expectDict(expr, 1u, "{ 1 ein }");
}
