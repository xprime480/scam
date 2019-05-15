#include "TestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class NumericParserTest : public TestBase
{
protected:
    NumericParserTest()
        : TestBase(false)
    {
    }
};

TEST_F(NumericParserTest, ZeroExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("0");
    expectInteger(expr, 0, "0", true);
}

TEST_F(NumericParserTest, ZeroInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("0.0");
    expectInteger(expr, 0, "0", false);
}

TEST_F(NumericParserTest, ZeroForceInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#i0");
    expectInteger(expr, 0, "0", false);
}

TEST_F(NumericParserTest, PositiveIntegerExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("42");
    expectInteger(expr, 42, "42", true);
}

TEST_F(NumericParserTest, PositiveIntegerInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("42.0");
    expectInteger(expr, 42, "42", false);
}

TEST_F(NumericParserTest, NegativeIntegerExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("-42");
    expectInteger(expr, -42, "-42", true);
}

TEST_F(NumericParserTest, NegativeIntegerInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("-42.0");
    expectInteger(expr, -42, "-42", false);
}

TEST_F(NumericParserTest, PositiveRealExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#e42.5");
    expectReal(expr, 42.5, "42.5", true);
}

TEST_F(NumericParserTest, PositiveRealInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("42.5");
    expectReal(expr, 42.5, "42.5", false);
}

TEST_F(NumericParserTest, NegativeRealExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#E-42.5");
    expectReal(expr, -42.5, "-42.5", true);
}

TEST_F(NumericParserTest, NegativeRealInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("-42.5");
    expectReal(expr, -42.5, "-42.5", false);
}

TEST_F(NumericParserTest, DecimalExplicitRadix)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#d#i17");
    expectInteger(expr, 17, "17", false);
}

TEST_F(NumericParserTest, BinaryPositiveInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#b101010");
    expectInteger(expr, 42, "42", true);
}

TEST_F(NumericParserTest, BinaryNegativeInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#B-101010");
    expectInteger(expr, -42, "-42", true);
}

TEST_F(NumericParserTest, OctalPositiveInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#o77");
    expectInteger(expr, 63, "63", true);
}

TEST_F(NumericParserTest, OctalNegativeInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#O#I-13");
    expectInteger(expr, -11, "-11", false);
}

TEST_F(NumericParserTest, HexadecimalPositiveInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#xFa13");
    expectInteger(expr, 64019, "64019", true);
}

TEST_F(NumericParserTest, HexadecimalNegativeInteger)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#X#I-FF");
    expectInteger(expr, -255, "-255", false);
}

TEST_F(NumericParserTest, NotANumberPositive)
{
    const char * text { "+nan.0" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectSpecialNumeric(expr, text);
}

TEST_F(NumericParserTest, NotANumberNegative)
{
    const char * text { "-nan.0" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(NumericParserTest, PlusInfinity)
{
    const char * text { "+inf.0" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectSpecialNumeric(expr, text);
}

TEST_F(NumericParserTest, MinusInfinity)
{
    const char * text { "-inf.0" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectSpecialNumeric(expr, text);
}

TEST_F(NumericParserTest, IntegerZeroExponent)
{
    const char * text { "5e0" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectInteger(expr, 5, "5", false);
}

TEST_F(NumericParserTest, IntegerPositiveExponent)
{
    const char * text { "5e1" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectInteger(expr, 50, "50", false);
}

TEST_F(NumericParserTest, IntegerNegativeExponent)
{
    const char * text { "5e-1" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectReal(expr, 0.5, "0.5", false);
}

TEST_F(NumericParserTest, RealZeroExponent)
{
    const char * text { "5.55e0" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectReal(expr, 5.55, "5.55", false);
}

TEST_F(NumericParserTest, RealPositiveExponent)
{
    const char * text { "-5.001e1" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectReal(expr, -50.01, "-50.01", false);
}

TEST_F(NumericParserTest, RealNegativeExponent)
{
    const char * text { "500.0e-2" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectInteger(expr, 5, "5", false);
}
