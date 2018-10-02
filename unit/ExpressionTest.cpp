
#include "ScamContext.hpp"
#include "ScamException.hpp"

#include "expr/ExpressionFactory.hpp"

#include "Extractor.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

ScamContext nullContext;

std::shared_ptr<ScamExpr> evaluate(shared_ptr<ScamExpr> input)
{
    shared_ptr<Extractor> ec = make_shared<Extractor>();
    ScamContext sc { ec };
    input->eval(sc);
    return ec->getExpr();
}

TEST(ExpressionTest, NullExpression)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeNull();

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ("null", expr->toString());

    EXPECT_TRUE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_FALSE(expr->truth());
    EXPECT_FALSE(expr->isNumeric());
    EXPECT_FALSE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);
    EXPECT_FALSE(expr->isString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr.get(), evaled.get());
    EXPECT_TRUE(evaled->isNull());
}

TEST(ExpressionTest, ErrorExpression)
{
    string const msg("Test message");

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeError(msg);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(msg, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_TRUE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_FALSE(expr->isNumeric());
    EXPECT_FALSE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);
    EXPECT_FALSE(expr->isString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toString(), evaled->toString());
}

void booleanTest(shared_ptr<ScamExpr> expr, bool value, string const & repr)
{
    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(repr, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_EQ(value, expr->truth());
    EXPECT_FALSE(expr->isNumeric());
    EXPECT_FALSE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);
    EXPECT_FALSE(expr->isString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr.get(), evaled.get());
    EXPECT_EQ(value, evaled->truth());
}

TEST(ExpressionTest, BooleanTrue)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeBoolean(true);
    booleanTest(expr, true, "#t");
}

TEST(ExpressionTest, BooleanFalse)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeBoolean(false);
    booleanTest(expr, false, "#f");
}

TEST(ExpressionTest, FloatTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeFloat(value);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(repr, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_TRUE(expr->isNumeric());
    EXPECT_TRUE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_EQ(value, expr->toFloat());
    EXPECT_THROW(expr->toInteger(), ScamException);
    EXPECT_FALSE(expr->isString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toFloat(), evaled->toFloat());
}

TEST(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeInteger(value);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(repr, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_TRUE(expr->isNumeric());
    EXPECT_TRUE(expr->isFloat());
    EXPECT_TRUE(expr->isInteger());
    EXPECT_EQ((double)value, expr->toFloat());
    EXPECT_EQ(value, expr->toInteger());
    EXPECT_FALSE(expr->isString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toInteger(), evaled->toInteger());
}

TEST(ExpressionTest, CharacterTest)
{
    string const value { "\\#Q" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCharacter(value);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(value, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_FALSE(expr->isNumeric());
    EXPECT_FALSE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_TRUE(expr->isChar());
    EXPECT_FALSE(expr->isString());
    EXPECT_EQ('Q', expr->toChar());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toChar(), evaled->toChar());
}

TEST(ExpressionTest, StringTest)
{
    string const value { "Fnord!" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeString(value);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(value, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_FALSE(expr->isNumeric());
    EXPECT_FALSE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_FALSE(expr->isChar());
    EXPECT_TRUE(expr->isString());
    EXPECT_EQ(value, expr->toString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toString(), evaled->toString());
}

TEST(ExpressionTest, NilTest)
{
    string const value { "()" };
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeNil();

    ASSERT_NE(nullptr, expr.get());
    EXPECT_EQ(value, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_TRUE(expr->truth());
    EXPECT_TRUE(expr->isNil());
    EXPECT_EQ(value, expr->toString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toString(), evaled->toString());
}
