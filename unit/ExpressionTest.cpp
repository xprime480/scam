
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
    ExprHandle expr = ExpressionFactory::makeNull();
    expectNull(expr);

    ExprHandle evaled = evaluate(expr.get());
    expectError(evaled);
}

TEST_F(ExpressionTest, ErrorExpression)
{
    string const msg("Test message");

    ExprHandle expr = ExpressionFactory::makeError(msg);
    expectError(expr, msg);

    ExprHandle evaled = evaluate(expr.get());
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

TEST_F(ExpressionTest, FloatTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    ExprHandle expr = ExpressionFactory::makeFloat(value);
    expectFloat(expr, value, repr);

    ExprHandle evaled = evaluate(expr.get());
    expectFloat(evaled, value, repr);
}

TEST_F(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    ExprHandle expr = ExpressionFactory::makeInteger(value);
    expectInteger(expr, value, repr);

    ExprHandle evaled = evaluate(expr.get());
    expectInteger(evaled, value, repr);
}

TEST_F(ExpressionTest, CharacterTest)
{
    string const repr { "\\#Q" };
    char value { 'Q' };

    ExprHandle expr = ExpressionFactory::makeCharacter(repr);
    expectChar(expr, value, repr);

    ExprHandle evaled = evaluate(expr.get());
    expectChar(evaled, value, repr);
}

TEST_F(ExpressionTest, StringTest)
{
    string const value { "Fnord!" };

    ExprHandle expr = ExpressionFactory::makeString(value);
    expectString(expr, value);

    ExprHandle evaled = evaluate(expr.get());
    expectString(evaled, value);
}

TEST_F(ExpressionTest, SymbolTest)
{
    string const name { "Fnord!" };

    ExprHandle sym = ExpressionFactory::makeSymbol(name);
    expectSymbol(sym, name);

    ExprHandle evaled = evaluate(sym.get());
    expectError(evaled);

    ExprHandle value = ExpressionFactory::makeInteger(1899);
    engine.addBinding(sym.get(), value.get());
    evaled = evaluate(sym.get());
    expectInteger(evaled, 1899, "1899");
}

TEST_F(ExpressionTest, NilTest)
{
    ExprHandle expr = ExpressionFactory::makeNil();
    expectNil(expr);

    ExprHandle evaled = evaluate(expr.get());
    expectNil(evaled);
}

TEST_F(ExpressionTest, ConsSingletonTest)
{
    string const value { "(works)" };

    ExprHandle car = ExpressionFactory::makeSymbol("works");
    ExprHandle cdr = ExpressionFactory::makeNil();
    ExprHandle expr = ExpressionFactory::makeCons(car.get(), cdr.get());

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
    ExprHandle cdr  = ExpressionFactory::makeCons(cadr.get(), cddr.get());;
    ExprHandle expr = ExpressionFactory::makeCons(car.get(), cdr.get());

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

    ExprHandle evaled = evaluate(expr.get());
    expectNil(evaled);
}

TEST_F(ExpressionTest, ListSingletonTest)
{
    string const value { "(works)" };

    ExprHandle car = ExpressionFactory::makeSymbol("works");
    ExprHandle expr = ExpressionFactory::makeList(car.get());

    expectList(expr, value, 1);

    ExprHandle first = expr->nthcar(0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ListDoubletonTest)
{
    string const value { "(works also)" };

    ExprHandle car0  = ExpressionFactory::makeSymbol("works");
    ExprHandle car1 = ExpressionFactory::makeSymbol("also");
    ExprHandle expr = ExpressionFactory::makeList(car0.get(), car1.get());

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

    ExprHandle car = ExpressionFactory::makeInteger(1);
    ExprHandle cdr = ExpressionFactory::makeInteger(2);
    ExprHandle expr = ExpressionFactory::makeCons(car.get(), cdr.get());

    expectCons(expr, value);

    expectInteger(expr->getCar(), 1, "1");
    expectInteger(expr->getCdr(), 2, "2");
}

TEST_F(ExpressionTest, ConsEvalTest)
{
    string const value { "(quote 2)" };

    ExprHandle car  = ExpressionFactory::makeSymbol("quote");
    ExprHandle cadr = ExpressionFactory::makeInteger(2);
    ExprHandle cddr = ExpressionFactory::makeNil();
    ExprHandle cdr  = ExpressionFactory::makeCons(cadr.get(), cddr.get());;
    ExprHandle expr = ExpressionFactory::makeCons(car.get(), cdr.get());

    expectList(expr, value, 2);
    expectSymbol(expr->getCar(), "quote");

    ExprHandle evaled = evaluate(expr.get());
    expectInteger(evaled, 2, "2");
}

TEST_F(ExpressionTest, ListCdrTest)
{
    ExprHandle one = ExpressionFactory::makeInteger(1);
    ExprHandle two = ExpressionFactory::makeInteger(2);
    ExprHandle three = ExpressionFactory::makeInteger(3);

    ExprHandle list = ExpressionFactory::makeList(one.get(),
                                                  two.get(),
                                                  three.get(),
                                                  two.get(),
                                                  one.get());
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
    ExprHandle one = ExpressionFactory::makeInteger(1);
    ExprHandle two = ExpressionFactory::makeInteger(2);
    ExprHandle three = ExpressionFactory::makeInteger(3);

    ExprHandle cons  = ExpressionFactory::makeCons(two.get(), three.get());
    ExprHandle plist = ExpressionFactory::makeCons(one.get(), cons.get());

    expectCons(plist, "(1 2 . 3)");

    ExprHandle cdr0 = plist->nthcdr(0);
    expectCons(cdr0, "(2 . 3)");

    ExprHandle cdr1 = plist->nthcdr(1);
    expectInteger(cdr1, 3, "3");

    ExprHandle cdr2 = plist->nthcdr(2);
    expectError(cdr2);
}

TEST_F(ExpressionTest, SpecialFormQuote)
{
    string const value { "Special Form quote" };

    ExprHandle quote  = ExpressionFactory::makeForm<Quote>();
    expectApplicable(quote, value);

    ExprHandle evaled = evaluate(quote.get());
    expectApplicable(evaled, value);
}

TEST_F(ExpressionTest, SpecialFormQuasiQuote)
{
    string const value { "Special Form quasiquote" };

    ExprHandle quote  = ExpressionFactory::makeForm<QuasiQuote>();
    expectApplicable(quote, value);

    ExprHandle evaled = evaluate(quote.get());
    expectApplicable(evaled, value);
}

TEST_F(ExpressionTest, VectorEmpty)
{
    string const value { "[]" };
    ExprVec vec;
    ExprHandle expr  = ExpressionFactory::makeVector(vec);
    expectVector(expr, value, 0);

    ExprHandle evaled = evaluate(expr.get());
    expectVector(evaled, value, 0);
}

TEST_F(ExpressionTest, VectorNonEmpty)
{
    string const value { "[1 2 3]" };
    ExprVec vec;
    for ( auto i : { 1, 2, 3 } ) {
        vec.push_back(ExpressionFactory::makeInteger(i));
    }

    auto f = [this, &value](ExprHandle expr) {
        expectVector(expr, value, 3u);
        expectInteger(expr->nthcar(0), 1, "1");
        expectInteger(expr->nthcar(1), 2, "2");
        expectInteger(expr->nthcar(2), 3, "3");
        expectError(expr->nthcar(3));
    };

    ExprHandle expr  = ExpressionFactory::makeVector(vec);
    f(expr);

    ExprHandle evaled = evaluate(expr.get());
    f(evaled);
}

TEST_F(ExpressionTest, CloneTest)
{
    ExprHandle e1 = ExpressionFactory::makeInteger(2932);
    ExprHandle e2 = e1->clone();

    EXPECT_EQ(e1.get(), e2.get());
}

TEST_F(ExpressionTest, GarbageCollection)
{
    try {
        unsigned i = 2 + ExpressionFactory::getMaxHandles();
        while ( i > 0 ) {
            ExpressionFactory::makeInteger(--i);
        }
    }
    catch ( ScamException e ) {
        FAIL() << "Unexpected scam exception: " << e.getMessage();
    }
}
