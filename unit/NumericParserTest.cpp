#include "TestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class NumericParserTest : public TestBase
{
};

TEST_F(NumericParserTest, ZeroExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("0");
    expectInteger(expr, 0, "0");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, ZeroInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("0.0");
    expectInteger(expr, 0, "0");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, ZeroForceInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#i0");
    expectInteger(expr, 0, "0");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, PositiveIntegerExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("42");
    expectInteger(expr, 42, "42");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, PositiveIntegerInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("42.0");
    expectInteger(expr, 42, "42");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, NegativeIntegerExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("-42");
    expectInteger(expr, -42, "-42");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, NegativeIntegerInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("-42.0");
    expectInteger(expr, -42, "-42");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, PositiveRealExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#e42.5");
    expectReal(expr, 42.5, "42.5");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, PositiveRealInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("42.5");
    expectReal(expr, 42.5, "42.5");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, NegativeRealExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#E-42.5");
    expectReal(expr, -42.5, "-42.5");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, NegativeRealInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("-42.5");
    expectReal(expr, -42.5, "-42.5");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, DecimalExplicitRadix)
{
    try {
        ExprHandle expr = ExpressionFactory::makeNumeric("#d#i17");
        expectInteger(expr, 17, "17");
        EXPECT_FALSE(expr->isExact());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage();
    }
}

TEST_F(NumericParserTest, BinaryPositiveInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#b101010");
    expectInteger(expr, 42, "42");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, BinaryNegativeInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#B-101010");
    expectInteger(expr, -42, "-42");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, OctalPositiveInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#o77");
    expectInteger(expr, 63, "63");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, OctalNegativeInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#O#I-13");
    expectInteger(expr, -11, "-11");
    EXPECT_FALSE(expr->isExact());
}

TEST_F(NumericParserTest, HexadecimalPositiveInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#xFa13");
    expectInteger(expr, 64019, "64019");
    EXPECT_TRUE(expr->isExact());
}

TEST_F(NumericParserTest, HexadecimalNegativeInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#X#I-FF");
    expectInteger(expr, -255, "-255");
    EXPECT_FALSE(expr->isExact());
}


