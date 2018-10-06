
#include "ExpressionTestBase.hpp"

#include "expr/ExpressionFactory.hpp"

#include "form/Quote.hpp"

using namespace std;
using namespace scam;

class ExpressionTest : public ExpressionTestBase
{
};

TEST_F(ExpressionTest, NullExpression)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeNull();
    expectNull(expr);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    expectError(evaled);
}

TEST_F(ExpressionTest, ErrorExpression)
{
    string const msg("Test message");

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeError(msg);
    expectError(expr, msg);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    expectError(evaled, msg);
}

TEST_F(ExpressionTest, BooleanTrue)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeBoolean(true);
    booleanTest(expr, true, "#t");
}

TEST_F(ExpressionTest, BooleanFalse)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeBoolean(false);
    booleanTest(expr, false, "#f");
}

TEST_F(ExpressionTest, FloatTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeFloat(value);
    expectFloat(expr, value, repr);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    expectFloat(evaled, value, repr);
}

TEST_F(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeInteger(value);
    expectInteger(expr, value, repr);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    expectInteger(evaled, value, repr);
}

TEST_F(ExpressionTest, CharacterTest)
{
    string const repr { "\\#Q" };
    char value { 'Q' };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCharacter(repr);
    expectChar(expr, value, repr);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    expectChar(evaled, value, repr);
}

TEST_F(ExpressionTest, StringTest)
{
    string const value { "Fnord!" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeString(value);
    expectString(expr, value);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    expectString(evaled, value);
}

TEST_F(ExpressionTest, SymbolTest)
{
    string const name { "Fnord!" };

    shared_ptr<ScamExpr> sym = ExpressionFactory::makeSymbol(name);
    expectSymbol(sym, name);

    shared_ptr<ScamExpr> evaled = evaluate(sym);
    expectError(evaled);

    shared_ptr<ScamExpr> value = ExpressionFactory::makeInteger(1899);
    env.put(sym, value);
    evaled = evaluate(sym);
    expectInteger(evaled, 1899, "1899");
}

TEST_F(ExpressionTest, NilTest)
{
    string const value { "()" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeNil();
    expectNil(expr, value);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    expectNil(evaled, value);
}

TEST_F(ExpressionTest, ConsSingletonTest)
{
    string const value { "(works)" };

    shared_ptr<ScamExpr> car = ExpressionFactory::makeSymbol("works");
    shared_ptr<ScamExpr> cdr = ExpressionFactory::makeNil();
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 1);

    shared_ptr<ScamExpr> first = expr->nth(0);
    expectSymbol(first, "works");
}

TEST_F(ExpressionTest, ConsDoubletonTest)
{
    string const value { "(works also)" };

    shared_ptr<ScamExpr> car  = ExpressionFactory::makeSymbol("works");
    shared_ptr<ScamExpr> cadr = ExpressionFactory::makeSymbol("also");
    shared_ptr<ScamExpr> cddr = ExpressionFactory::makeNil();
    shared_ptr<ScamExpr> cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 2);

    shared_ptr<ScamExpr> first = expr->nth(0);
    expectSymbol(first, "works");

    shared_ptr<ScamExpr> second = expr->nth(1);
    expectSymbol(second, "also");

    shared_ptr<ScamExpr> third = expr->nth(2);
    expectError(third);

    shared_ptr<ScamExpr> car2 = expr->getCar();
    expectSymbol(car2, "works");

    shared_ptr<ScamExpr> cdr2 = expr->getCdr();
    expectList(cdr2, "(also)", 1);
}

TEST_F(ExpressionTest, ConsDottedPair)
{
    string const value { "(1 . 2)" };

    shared_ptr<ScamExpr> car = ExpressionFactory::makeInteger(1);
    shared_ptr<ScamExpr> cdr = ExpressionFactory::makeInteger(2);
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCons(car, cdr);

    expectCons(expr, value);

    expectInteger(expr->getCar(), 1, "1");
    expectInteger(expr->getCdr(), 2, "2");
}

TEST_F(ExpressionTest, ConsEvalTest)
{
    string const value { "(quote 2)" };

    shared_ptr<ScamExpr> car  = ExpressionFactory::makeSymbol("quote");
    shared_ptr<ScamExpr> cadr = ExpressionFactory::makeInteger(2);
    shared_ptr<ScamExpr> cddr = ExpressionFactory::makeNil();
    shared_ptr<ScamExpr> cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCons(car, cdr);

    expectList(expr, value, 2);
    expectSymbol(expr->getCar(), "quote");

    shared_ptr<ScamExpr> quote = ExpressionFactory::makeForm<Quote>();
    env.put(car, quote);
    shared_ptr<ScamExpr> evaled = evaluate(expr);

    expectInteger(evaled, 2, "2");
}

TEST_F(ExpressionTest, SpecialFormQuote)
{
    string const value { "Special Form quote" };

    shared_ptr<ScamExpr> quote  = ExpressionFactory::makeForm<Quote>();
    expectApplicable(quote, value);

    shared_ptr<ScamExpr> evaled = evaluate(quote);
    expectApplicable(evaled, value);
}

TEST_F(ExpressionTest, VectorEmpty)
{
    string const value { "[]" };
    vector<shared_ptr<ScamExpr>> vec;
    shared_ptr<ScamExpr> expr  = ExpressionFactory::makeVector(vec);
    expectVector(expr, value, 0);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    expectVector(evaled, value, 0);
}

TEST_F(ExpressionTest, VectorNonEmpty)
{
    string const value { "[1 2 3]" };
    vector<shared_ptr<ScamExpr>> vec;
    for ( auto i : { 1, 2, 3 } ) {
        vec.push_back(ExpressionFactory::makeInteger(i));
    }

    auto f = [this, &value](shared_ptr<ScamExpr> & expr) {
        expectVector(expr, value, 3u);
        expectInteger(expr->nth(0), 1, "1");
        expectInteger(expr->nth(1), 2, "2");
        expectInteger(expr->nth(2), 3, "3");
        expectError(expr->nth(3));
    };

    shared_ptr<ScamExpr> expr  = ExpressionFactory::makeVector(vec);
    f(expr);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    f(evaled);
}
