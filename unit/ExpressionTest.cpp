
#include "ScamException.hpp"

#include "Env.hpp"
#include "Extractor.hpp"

#include "expr/ExpressionFactory.hpp"
#include "form/Quote.hpp"


#include "gtest/gtest.h"

using namespace std;
using namespace scam;

namespace
{
    static const unsigned long SELECT_NULL    { 1 << 0 };
    static const unsigned long SELECT_ERROR   { 1 << 1 };
    static const unsigned long SELECT_TRUTH   { 1 << 2 };
    static const unsigned long SELECT_CHAR    { 1 << 3 };
    static const unsigned long SELECT_STRING  { 1 << 4 };
    static const unsigned long SELECT_SYMBOL  { 1 << 5 };
    static const unsigned long SELECT_NUMERIC { 1 << 6 };
    static const unsigned long SELECT_FLOAT   { 1 << 7 };
    static const unsigned long SELECT_INTEGER { 1 << 8 };
    static const unsigned long SELECT_BOOLEAN { 1 << 9 };
    static const unsigned long SELECT_NIL     { 1 << 10 };
    static const unsigned long SELECT_CONS    { 1 << 11 };
    static const unsigned long SELECT_LIST    { 1 << 12 };
    static const unsigned long SELECT_VECTOR  { 1 << 13 };
    static const unsigned long SELECT_APPLY   { 1 << 14 };

    static const unsigned long ALL_FLOAT   = SELECT_NUMERIC | SELECT_FLOAT;
    static const unsigned long ALL_INTEGER = ALL_FLOAT | SELECT_INTEGER;
    static const unsigned long ALL_NIL     = SELECT_NIL | SELECT_LIST;
}

class ExpressionTest : public ::testing::Test
{
protected:
    ExpressionTest()
        : extractor(make_shared<Extractor>())
    {
    }

    shared_ptr<Extractor> extractor;
    Env env;

    std::shared_ptr<ScamExpr> evaluate(shared_ptr<ScamExpr> input)
    {
        input->eval(extractor, env);
        return extractor->getExpr();
    }

    void booleanTest(shared_ptr<ScamExpr> expr, bool value, string const & repr)
    {
        EXPECT_EQ(repr, expr->toString());
        checkPredicates(expr, SELECT_BOOLEAN | (value ? SELECT_TRUTH : 0));

        EXPECT_THROW(expr->toFloat(), ScamException);
        EXPECT_THROW(expr->toInteger(), ScamException);

        shared_ptr<ScamExpr> evaled = evaluate(expr);
        EXPECT_EQ(expr.get(), evaled.get());
        checkPredicates(evaled, SELECT_BOOLEAN | (value ? SELECT_TRUTH : 0));
    }

    void doCheck(bool act, unsigned selector, unsigned which)
    {
        bool exp = (selector & which) == which;
        EXPECT_EQ(exp, act) << "failed for " << which;
    }

    void checkPredicates(shared_ptr<ScamExpr> expr, unsigned selector)
    {
        ASSERT_NE(nullptr, expr.get());

        doCheck(expr->isNull(), selector, SELECT_NULL);
        doCheck(expr->error(),  selector, SELECT_ERROR);
        doCheck(expr->truth(),  selector, SELECT_TRUTH);

        doCheck(expr->isBoolean(), selector, SELECT_BOOLEAN);
        doCheck(expr->isChar(),    selector, SELECT_CHAR);
        doCheck(expr->isString(),  selector, SELECT_STRING);
        doCheck(expr->isSymbol(),  selector, SELECT_SYMBOL);

        doCheck(expr->isNumeric(), selector, SELECT_NUMERIC);
        doCheck(expr->isFloat(),   selector, SELECT_FLOAT);
        doCheck(expr->isInteger(), selector, SELECT_INTEGER);

        doCheck(expr->isNil(),    selector, SELECT_NIL);
        doCheck(expr->isCons(),   selector, SELECT_CONS);
        doCheck(expr->isList(),   selector, SELECT_LIST);
        doCheck(expr->isVector(), selector, SELECT_VECTOR);

        doCheck(expr->hasApply(), selector, SELECT_APPLY);
    }
};

TEST_F(ExpressionTest, NullExpression)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeNull();

    checkPredicates(expr, SELECT_NULL);
    EXPECT_EQ("null", expr->toString());
    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(evaled, SELECT_ERROR | SELECT_TRUTH);
}

TEST_F(ExpressionTest, ErrorExpression)
{
    string const msg("Test message");

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeError(msg);
    checkPredicates(expr, SELECT_ERROR | SELECT_TRUTH);
    EXPECT_EQ(msg, expr->toString());
    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(expr, SELECT_ERROR | SELECT_TRUTH);
    EXPECT_EQ(expr->toString(), evaled->toString());
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
    checkPredicates(expr, SELECT_TRUTH | ALL_FLOAT);
    EXPECT_EQ(repr, expr->toString());

    EXPECT_EQ(value, expr->toFloat());
    EXPECT_THROW(expr->toInteger(), ScamException);

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(evaled, SELECT_TRUTH | ALL_FLOAT);
    EXPECT_EQ(expr->toFloat(), evaled->toFloat());
}

TEST_F(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeInteger(value);
    checkPredicates(expr, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ((double)value, expr->toFloat());
    EXPECT_EQ(value, expr->toInteger());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(evaled, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ(expr->toInteger(), evaled->toInteger());
}

TEST_F(ExpressionTest, CharacterTest)
{
    string const value { "\\#Q" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCharacter(value);
    checkPredicates(expr, SELECT_TRUTH | SELECT_CHAR);
    EXPECT_EQ(value, expr->toString());
    EXPECT_EQ('Q', expr->toChar());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(evaled, SELECT_TRUTH | SELECT_CHAR);
    EXPECT_EQ(expr->toChar(), evaled->toChar());
}

TEST_F(ExpressionTest, StringTest)
{
    string const value { "Fnord!" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeString(value);
    checkPredicates(expr, SELECT_TRUTH | SELECT_STRING);
    EXPECT_EQ(value, expr->toString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(evaled, SELECT_TRUTH | SELECT_STRING);
    EXPECT_EQ(expr->toString(), evaled->toString());
}

TEST_F(ExpressionTest, SymbolTest)
{
    string const name { "Fnord!" };

    shared_ptr<ScamExpr> sym = ExpressionFactory::makeSymbol(name);
    checkPredicates(sym, SELECT_TRUTH | SELECT_SYMBOL);
    EXPECT_EQ(name, sym->toString());

    shared_ptr<ScamExpr> evaled = evaluate(sym);
    checkPredicates(evaled, SELECT_TRUTH | SELECT_ERROR);

    shared_ptr<ScamExpr> value = ExpressionFactory::makeInteger(1899);
    env.put(sym, value);
    evaled = evaluate(sym);
    checkPredicates(evaled, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ(value->toInteger(), evaled->toInteger());
}

TEST_F(ExpressionTest, NilTest)
{
    string const value { "()" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeNil();
    checkPredicates(expr, SELECT_TRUTH | ALL_NIL);
    EXPECT_EQ(value, expr->toString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(evaled, SELECT_TRUTH | ALL_NIL);
    EXPECT_EQ(expr->toString(), evaled->toString());
}

TEST_F(ExpressionTest, ConsSingletonTest)
{
    string const value { "(works)" };

    shared_ptr<ScamExpr> car = ExpressionFactory::makeSymbol("works");
    shared_ptr<ScamExpr> cdr = ExpressionFactory::makeNil();
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCons(car, cdr);

    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS | SELECT_LIST);
    EXPECT_EQ(value, expr->toString());

    EXPECT_EQ(1, expr->length());
    shared_ptr<ScamExpr> first = expr->nth(0);
    EXPECT_EQ("works", first->toString());
}

TEST_F(ExpressionTest, ConsDoubletonTest)
{
    string const value { "(works also)" };

    shared_ptr<ScamExpr> car  = ExpressionFactory::makeSymbol("works");
    shared_ptr<ScamExpr> cadr = ExpressionFactory::makeSymbol("also");
    shared_ptr<ScamExpr> cddr = ExpressionFactory::makeNil();
    shared_ptr<ScamExpr> cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCons(car, cdr);

    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS | SELECT_LIST);
    EXPECT_EQ(value, expr->toString());

    shared_ptr<ScamExpr> car2 = expr->getCar();
    checkPredicates(car2, SELECT_TRUTH | SELECT_SYMBOL);
    EXPECT_EQ("works", car2->toString());
}

TEST_F(ExpressionTest, ConsDottedPair)
{
    string const value { "(1 . 2)" };

    shared_ptr<ScamExpr> car = ExpressionFactory::makeInteger(1);
    shared_ptr<ScamExpr> cdr = ExpressionFactory::makeInteger(2);
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCons(car, cdr);

    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS);
    EXPECT_EQ(value, expr->toString());
}

TEST_F(ExpressionTest, ConsEvalTest)
{
    string const value { "(quote 2)" };

    shared_ptr<ScamExpr> car  = ExpressionFactory::makeSymbol("quote");
    shared_ptr<ScamExpr> cadr = ExpressionFactory::makeInteger(2);
    shared_ptr<ScamExpr> cddr = ExpressionFactory::makeNil();
    shared_ptr<ScamExpr> cdr  = ExpressionFactory::makeCons(cadr, cddr);;
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCons(car, cdr);

    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS | SELECT_LIST);
    EXPECT_EQ(value, expr->toString());

    shared_ptr<ScamExpr> car2 = expr->getCar();
    checkPredicates(car2, SELECT_TRUTH | SELECT_SYMBOL);
    EXPECT_EQ("quote", car2->toString());

    shared_ptr<ScamExpr> quote = ExpressionFactory::makeForm<Quote>();
    env.put(car, quote);
    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(evaled, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ("2", evaled->toString());
}

TEST_F(ExpressionTest, SpecialFormQuote)
{
    string const value { "Special Form quote" };

    shared_ptr<ScamExpr> quote  = ExpressionFactory::makeForm<Quote>();

    checkPredicates(quote, SELECT_TRUTH | SELECT_APPLY);
    EXPECT_EQ(value, quote->toString());

    shared_ptr<ScamExpr> evaled = evaluate(quote);
    checkPredicates(evaled, SELECT_TRUTH | SELECT_APPLY);
}

TEST_F(ExpressionTest, VectorEmpty)
{
    string const value { "[]" };
    vector<shared_ptr<ScamExpr>> vec;
    shared_ptr<ScamExpr> expr  = ExpressionFactory::makeVector(vec);

    checkPredicates(expr, SELECT_TRUTH | SELECT_VECTOR);
    EXPECT_EQ(value, expr->toString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(evaled, SELECT_TRUTH | SELECT_VECTOR);
    EXPECT_EQ(value, evaled->toString());
}

TEST_F(ExpressionTest, VectorNonEmpty)
{
    string const value { "[1 2 3]" };
    vector<shared_ptr<ScamExpr>> vec;
    for ( auto i : { 1, 2, 3 } ) {
        vec.push_back(ExpressionFactory::makeInteger(i));
    }
    shared_ptr<ScamExpr> expr  = ExpressionFactory::makeVector(vec);

    checkPredicates(expr, SELECT_TRUTH | SELECT_VECTOR);
    EXPECT_EQ(value, expr->toString());
    EXPECT_EQ(3, expr->length());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    checkPredicates(evaled, SELECT_TRUTH | SELECT_VECTOR);
    EXPECT_EQ(value, evaled->toString());

    shared_ptr<ScamExpr> third = expr->nth(2);
    checkPredicates(third, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ(3, third->toInteger());

    shared_ptr<ScamExpr> fourth = expr->nth(3);
    checkPredicates(fourth, SELECT_TRUTH | SELECT_ERROR);
}
