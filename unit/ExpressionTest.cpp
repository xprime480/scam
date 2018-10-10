
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

    ExprHandle evaled = evaluate(expr);
    expectError(evaled);
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

TEST_F(ExpressionTest, FloatTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    ExprHandle expr = ExpressionFactory::makeFloat(value);
    expectFloat(expr, value, repr);

    ExprHandle evaled = evaluate(expr);
    expectFloat(evaled, value, repr);
}

TEST_F(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    ExprHandle expr = ExpressionFactory::makeInteger(value);
    expectInteger(expr, value, repr);

    ExprHandle evaled = evaluate(expr);
    expectInteger(evaled, value, repr);
}

TEST_F(ExpressionTest, CharacterTest)
{
    string const repr { "\\#Q" };
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

    ExprHandle value = ExpressionFactory::makeInteger(1899);
    env.put(sym, value);
    evaled = evaluate(sym);
    expectInteger(evaled, 1899, "1899");
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

    ExprHandle first = expr->nth(0);
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

    ExprHandle first = expr->nth(0);
    expectSymbol(first, "works");

    ExprHandle second = expr->nth(1);
    expectSymbol(second, "also");

    ExprHandle third = expr->nth(2);
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
    ExprHandle expr = ExpressionFactory::makeCons(car, cdr);

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
    ExprHandle cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    ExprHandle expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 2);
    expectSymbol(expr->getCar(), "quote");

    ExprHandle evaled = evaluate(expr);
    expectInteger(evaled, 2, "2");
}

TEST_F(ExpressionTest, SpecialFormQuote)
{
    string const value { "Special Form quote" };

    ExprHandle quote  = ExpressionFactory::makeForm<Quote>();
    expectApplicable(quote, value);

    ExprHandle evaled = evaluate(quote);
    expectApplicable(evaled, value);
}

TEST_F(ExpressionTest, VectorEmpty)
{
    string const value { "[]" };
    ExprVec vec;
    ExprHandle expr  = ExpressionFactory::makeVector(vec);
    expectVector(expr, value, 0);

    ExprHandle evaled = evaluate(expr);
    expectVector(evaled, value, 0);
}

TEST_F(ExpressionTest, VectorNonEmpty)
{
    string const value { "[1 2 3]" };
    ExprVec vec;
    for ( auto i : { 1, 2, 3 } ) {
        vec.push_back(ExpressionFactory::makeInteger(i));
    }

    auto f = [this, &value](ExprHandle & expr) {
        expectVector(expr, value, 3u);
        expectInteger(expr->nth(0), 1, "1");
        expectInteger(expr->nth(1), 2, "2");
        expectInteger(expr->nth(2), 3, "3");
        expectError(expr->nth(3));
    };

    ExprHandle expr  = ExpressionFactory::makeVector(vec);
    f(expr);

    ExprHandle evaled = evaluate(expr);
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
