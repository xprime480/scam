#include "TestBase.hpp"

using namespace std;
using namespace scam;

class NumericParserTest : public TestBase
{
};

TEST_F(NumericParserTest, ZeroExact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("0");
    expectInteger(expr, 0, "0");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, ZeroInexact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("0.0");
    expectInteger(expr, 0, "0");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, ZeroForceInexact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("#i0");
    expectInteger(expr, 0, "0");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, PositiveIntegerExact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("42");
    expectInteger(expr, 42, "42");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, PositiveIntegerInexact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("42.0");
    expectInteger(expr, 42, "42");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, NegativeIntegerExact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("-42");
    expectInteger(expr, -42, "-42");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, NegativeIntegerInexact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("-42.0");
    expectInteger(expr, -42, "-42");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, PositiveRealExact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("#e42.5");
    expectReal(expr, 42.5, "42.5");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, PositiveRealInexact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("42.5");
    expectReal(expr, 42.5, "42.5");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, NegativeRealExact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("#E-42.5");
    expectReal(expr, -42.5, "-42.5");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, NegativeRealInexact)
{
    ScamNumeric * expr = ExpressionFactory::makeNumeric("-42.5");
    expectReal(expr, -42.5, "-42.5");
    EXPECT_FALSE(expr->isExact());
}
