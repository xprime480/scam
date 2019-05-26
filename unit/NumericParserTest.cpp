#include "TestBase.hpp"

#include "ScamException.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/ValueFactory.hpp"
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
    ScamValue expr = makeNumeric("0");
    expectInteger(expr, 0, "0", true);
}

TEST_F(NumericParserTest, ZeroInexact)
{
    ScamValue expr = makeNumeric("0.0");
    expectInteger(expr, 0, "0", false);
}

TEST_F(NumericParserTest, ZeroForceInexact)
{
    ScamValue expr = makeNumeric("#i0");
    expectInteger(expr, 0, "0", false);
}

TEST_F(NumericParserTest, PositiveIntegerExact)
{
    ScamValue expr = makeNumeric("42");
    expectInteger(expr, 42, "42", true);
}

TEST_F(NumericParserTest, PositiveIntegerInexact)
{
    ScamValue expr = makeNumeric("42.0");
    expectInteger(expr, 42, "42", false);
}

TEST_F(NumericParserTest, NegativeIntegerExact)
{
    ScamValue expr = makeNumeric("-42");
    expectInteger(expr, -42, "-42", true);
}

TEST_F(NumericParserTest, NegativeIntegerInexact)
{
    ScamValue expr = makeNumeric("-42.0");
    expectInteger(expr, -42, "-42", false);
}

TEST_F(NumericParserTest, PositiveRationalExact)
{
    ScamValue expr = makeNumeric("22/7");
    RationalPair value { 22, 7 };
    expectRational(expr, value, "22/7", true);
}

TEST_F(NumericParserTest, PositiveRationalInexact)
{
    ScamValue expr = makeNumeric("#i22/7");
    RationalPair value { 22, 7 };
    expectRational(expr, value, "22/7", false);
}

TEST_F(NumericParserTest, NegativeRationalExact)
{
    ScamValue expr = makeNumeric("-22/7");
    RationalPair value { -22, 7 };
    expectRational(expr, value, "-22/7", true);
}

TEST_F(NumericParserTest, NegativeRationalInexact)
{
    ScamValue expr = makeNumeric("#I-22/7");
    RationalPair value { -22, 7 };
    expectRational(expr, value, "-22/7", false);
}

TEST_F(NumericParserTest, HexadecimalRational)
{
    ScamValue expr = makeNumeric("#x3/10");
    RationalPair value { 3, 16 };
    expectRational(expr, value, "3/16", true);
}

TEST_F(NumericParserTest, RationalLowestTerms)
{
    ScamValue expr = makeNumeric("50/100");
    RationalPair value { 1, 2 };
    expectRational(expr, value, "1/2", true);
}

TEST_F(NumericParserTest, RationalWithUnitDenominator)
{
    ScamValue expr = makeNumeric("100/50");
    expectInteger(expr, 2, "2", true);
}

TEST_F(NumericParserTest, PositiveRealExact)
{
    ScamValue expr = makeNumeric("#e42.5000000");
    expectReal(expr, 42.5, "42.5", true);
}

TEST_F(NumericParserTest, PositiveRealInexact)
{
    ScamValue expr = makeNumeric("42.5000000");
    expectReal(expr, 42.5, "42.5", false);
}

TEST_F(NumericParserTest, NegativeRealExact)
{
    ScamValue expr = makeNumeric("#E-42.5000000");
    expectReal(expr, -42.5, "-42.5", true);
}

TEST_F(NumericParserTest, NegativeRealInexact)
{
    ScamValue expr = makeNumeric("-42.5000000");
    expectReal(expr, -42.5, "-42.5", false);
}

TEST_F(NumericParserTest, DecimalExplicitRadix)
{
    ScamValue expr = makeNumeric("#d#i17");
    expectInteger(expr, 17, "17", false);
}

TEST_F(NumericParserTest, BinaryPositiveInteger)
{
    ScamValue expr = makeNumeric("#b101010");
    expectInteger(expr, 42, "42", true);
}

TEST_F(NumericParserTest, BinaryNegativeInteger)
{
    ScamValue expr = makeNumeric("#B-101010");
    expectInteger(expr, -42, "-42", true);
}

TEST_F(NumericParserTest, OctalPositiveInteger)
{
    ScamValue expr = makeNumeric("#o77");
    expectInteger(expr, 63, "63", true);
}

TEST_F(NumericParserTest, OctalNegativeInteger)
{
    ScamValue expr = makeNumeric("#O#I-13");
    expectInteger(expr, -11, "-11", false);
}

TEST_F(NumericParserTest, HexadecimalPositiveInteger)
{
    ScamValue expr = makeNumeric("#xFa13");
    expectInteger(expr, 64019, "64019", true);
}

TEST_F(NumericParserTest, HexadecimalNegativeInteger)
{
    ScamValue expr = makeNumeric("#X#I-FF");
    expectInteger(expr, -255, "-255", false);
}

TEST_F(NumericParserTest, NotANumberPositive)
{
    const char * text { "+nan.0" };
    ScamValue expr = makeNumeric(text);
    expectSpecialNumeric(expr, text);
}

TEST_F(NumericParserTest, NotANumberNegative)
{
    const char * text { "-nan.0" };
    ScamValue expr = makeNumeric(text);
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(NumericParserTest, PlusInfinity)
{
    const char * text { "+inf.0" };
    ScamValue expr = makeNumeric(text);
    expectSpecialNumeric(expr, text);
}

TEST_F(NumericParserTest, MinusInfinity)
{
    const char * text { "-inf.0" };
    ScamValue expr = makeNumeric(text);
    expectSpecialNumeric(expr, text);
}

TEST_F(NumericParserTest, IntegerZeroExponent)
{
    const char * text { "5e0" };
    ScamValue expr = makeNumeric(text);
    expectInteger(expr, 5, "5", false);
}

TEST_F(NumericParserTest, IntegerPositiveExponent)
{
    const char * text { "5e1" };
    ScamValue expr = makeNumeric(text);
    expectInteger(expr, 50, "50", false);
}

TEST_F(NumericParserTest, IntegerNegativeExponent)
{
    const char * text { "5e-1" };
    ScamValue expr = makeNumeric(text);
    expectReal(expr, 0.5, "0.5", false);
}

TEST_F(NumericParserTest, RealZeroExponent)
{
    const char * text { "5.5500000e0" };
    ScamValue expr = makeNumeric(text);
    expectReal(expr, 5.55, "5.55", false);
}

TEST_F(NumericParserTest, RealPositiveExponent)
{
    const char * text { "-5.0010000e1" };
    ScamValue expr = makeNumeric(text);
    expectReal(expr, -50.01, "-50.01", false);
}

TEST_F(NumericParserTest, RealNegativeExponent)
{
    const char * text { "500.0e-2" };
    ScamValue expr = makeNumeric(text);
    expectInteger(expr, 5, "5", false);
}

TEST_F(NumericParserTest, ComplexSimpleParts)
{
    const char * text { "5+3i" };
    ScamValue expr = makeNumeric(text);
    ScamValue null  { makeNull() };
    expectComplex(expr, null, null, text, true);
}

TEST_F(NumericParserTest, ComplexSimplePartsNegativeImaginary)
{
    const char * text { "5-3i" };
    ScamValue expr = makeNumeric(text);
    ScamValue null  { makeNull() };
    expectComplex(expr, null, null, text, true);
}

TEST_F(NumericParserTest, ComplexInfiniteImaginary)
{
    const char * text { "5-inf.0i" };
    ScamValue expr = makeNumeric(text);
    ScamValue null  { makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexNanImaginary)
{
    const char * text { "5+nan.0i" };
    ScamValue expr = makeNumeric(text);
    ScamValue null  { makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexJustNanImaginary)
{
    const char * text { "+nan.0i" };
    ScamValue expr = makeNumeric(text);
    ScamValue null  { makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexMinusI)
{
    const char * text { "-i" };
    ScamValue expr = makeNumeric(text);
    ScamValue null  { makeNull() };
    expectComplex(expr, null, null, text, true);
}

TEST_F(NumericParserTest, ComplexCompletelyNan)
{
    const char * text { "+nan.0+nan.0i" };
    ScamValue expr = makeNumeric(text);
    ScamValue null  { makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexInfRealFiniteImag)
{
    const char * text { "-inf.0-7i" };
    ScamValue expr = makeNumeric(text);
    ScamValue null  { makeNull() };
    expectComplex(expr, null, null, text, false);
}

TEST_F(NumericParserTest, ComplexPolarForm)
{
    const char * text { "1.41421356237@-0.78539816339" };
    const char * repr { "1-i" };
    ScamValue expr = makeNumeric(text);
    ScamValue null  { makeNull() };
    expectComplex(expr, null, null, repr, false);
}

TEST_F(NumericParserTest, RealFractionVeryLong)
{
    const char * text { "-0.78539816339" };
    const char * repr { "-0.785398" };
    ScamValue expr = makeNumeric(text);
    expectReal(expr, -0.78539816339, repr, false);
}
