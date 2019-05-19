#include "TestBase.hpp"

#include "ScamException.hpp"
#include "util/NumericConverter.hpp"

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

TEST_F(NumericParserTest, PositiveRationalExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("22/7");
    expectRational(expr, make_pair<int, int>(22, 7), "22/7", true);
}

TEST_F(NumericParserTest, PositiveRationalInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#i22/7");
    expectRational(expr, make_pair<int, int>(22, 7), "22/7", false);
}

TEST_F(NumericParserTest, NegativeRationalExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("-22/7");
    expectRational(expr, make_pair<int, int>(-22, 7), "-22/7", true);
}

TEST_F(NumericParserTest, NegativeRationalInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#I-22/7");
    expectRational(expr, make_pair<int, int>(-22, 7), "-22/7", false);
}

TEST_F(NumericParserTest, HexadecimalRational)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#x3/10");
    expectRational(expr, make_pair<int, int>(3, 16), "3/16", true);
}

TEST_F(NumericParserTest, RationalLowestTerms)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("50/100");
    expectRational(expr, make_pair<int, int>(1, 2), "1/2", true);
}

TEST_F(NumericParserTest, RationalWithUnitDenominator)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("100/50");
    expectInteger(expr, 2, "2", true);
}

TEST_F(NumericParserTest, PositiveRealExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#e42.5000000");
    expectReal(expr, 42.5, "42.5", true);
}

TEST_F(NumericParserTest, PositiveRealInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("42.5000000");
    expectReal(expr, 42.5, "42.5", false);
}

TEST_F(NumericParserTest, NegativeRealExact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("#E-42.5000000");
    expectReal(expr, -42.5, "-42.5", true);
}

TEST_F(NumericParserTest, NegativeRealInexact)
{
    ExprHandle expr = ExpressionFactory::makeNumeric("-42.5000000");
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
    const char * text { "5.5500000e0" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectReal(expr, 5.55, "5.55", false);
}

TEST_F(NumericParserTest, RealPositiveExponent)
{
    const char * text { "-5.0010000e1" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectReal(expr, -50.01, "-50.01", false);
}

TEST_F(NumericParserTest, RealNegativeExponent)
{
    const char * text { "500.0e-2" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectInteger(expr, 5, "5", false);
}

TEST_F(NumericParserTest, ComplexSimpleParts)
{
    const char * text { "5+3i" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    ExprHandle null  { ExpressionFactory::makeNull() };
    expectComplex(expr, null, null, text, true);
}

TEST_F(NumericParserTest, ComplexSimplePartsNegativeImaginary)
{
    const char * text { "5-3i" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    ExprHandle null  { ExpressionFactory::makeNull() };
    expectComplex(expr, null, null, text, true);
}

TEST_F(NumericParserTest, ComplexInfiniteImaginary)
{
    const char * text { "5-inf.0i" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    ExprHandle null  { ExpressionFactory::makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexNanImaginary)
{
    const char * text { "5+nan.0i" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    ExprHandle null  { ExpressionFactory::makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexJustNanImaginary)
{
    const char * text { "+nan.0i" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    ExprHandle null  { ExpressionFactory::makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexMinusI)
{
    const char * text { "-i" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    ExprHandle null  { ExpressionFactory::makeNull() };
    expectComplex(expr, null, null, text, true);
}

TEST_F(NumericParserTest, ComplexCompletelyNan)
{
    const char * text { "+nan.0+nan.0i" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    ExprHandle null  { ExpressionFactory::makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexInfRealFiniteImag)
{
    const char * text { "-inf.0-7i" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    ExprHandle null  { ExpressionFactory::makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexPolarForm)
{
    const char * text { "1.41421356237@-0.78539816339" };
    const char * repr { "1-i" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    ExprHandle null  { ExpressionFactory::makeNull() };
    expectComplex(expr, null, null, repr, false);
}

TEST_F(NumericParserTest, RealFractionVeryLong)
{
    const char * text { "-0.78539816339" };
    const char * repr { "-0.785398" };
    ExprHandle expr = ExpressionFactory::makeNumeric(text);
    expectReal(expr, -0.78539816339, repr, false);
}
