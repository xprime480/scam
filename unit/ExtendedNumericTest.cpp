#include "TestBase.hpp"

#include "expr/EqualityOps.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "value/ValueFactory.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

class ExtendedNumericTest : public TestBase
{
protected:
    ScamValue nanValue;
    ScamValue negInfValue;
    ScamValue posInfValue;
    ScamValue intValue1;
    ScamValue intValue2;
    ScamValue rationalValue1;
    ScamValue rationalValue2;
    ScamValue realValue1;
    ScamValue realValue2;
    ScamValue complexValue1;

    ExtendedNumericTest()
        : nanValue(makeNaN())
        , negInfValue(makeNegInf())
        , posInfValue(makePosInf())
        , intValue1(makeInteger(2, true))
        , intValue2(makeInteger(5, true))
        , rationalValue1(makeRational(1, 2, true))
        , rationalValue2(makeRational(3, 4, true))
        , realValue1(makeReal(1.2, true))
        , realValue2(makeReal(5.0, true))
        , complexValue1(makeComplex(intValue1, realValue2))
    {
    }

    void createAndCompareTest(ScamValue value)
    {
        ExtendedNumeric extValue(value);
        ScamValue test = extValue.get();
        ExtendedNumeric extTest(test);

        EXPECT_EQ(test, value);

        if ( isNaN(value) ) {
            EXPECT_FALSE(extValue == extTest);
            EXPECT_FALSE(extValue <= extTest);
            EXPECT_FALSE(extValue >= extTest);
        }
        else {
            EXPECT_TRUE(extValue == extTest);
            EXPECT_TRUE(extValue <= extTest);
            EXPECT_TRUE(extValue >= extTest);
        }

        EXPECT_FALSE(extValue < extTest);
        EXPECT_FALSE(extValue > extTest);
        EXPECT_FALSE(extValue != extTest);
    }

    void compareALessBTest(ScamValue a, ScamValue b)
    {
        ExtendedNumeric extA(a);
        ExtendedNumeric extB(b);

        if ( isNaN(a) || isNaN(b) ) {
            EXPECT_FALSE(extA <  extB);
            EXPECT_FALSE(extA <= extB);
            EXPECT_FALSE(extA != extB);
        }
        else {
            EXPECT_TRUE(extA <  extB) << writeValue(a) << " " << writeValue(b);
            EXPECT_TRUE(extA <= extB);
            EXPECT_TRUE(extA != extB);
        }

        EXPECT_FALSE(extA == extB);
        EXPECT_FALSE(extA >  extB);
        EXPECT_FALSE(extA >= extB);
    }

    void check4(ScamValue a, ScamValue b, ScamValue exp, ScamValue act)
    {
        if ( isNaN(a) || isNaN(b) || isNaN(exp) ) {
            EXPECT_TRUE(isNaN(act));
        }
        else {
            EXPECT_TRUE(equals(exp, act)) << writeValue(a) << " "
                                          << writeValue(b) << " "
                                          << writeValue(exp) << " "
                                          << writeValue(act);
        }
    }

    void additionTest(ScamValue a, ScamValue b, ScamValue exp = makeNothing())
    {
        ExtendedNumeric extA(a);
        ExtendedNumeric extB(b);
        ExtendedNumeric extC { extA + extB };
        ScamValue act = extC.get();

        check4(a, b, exp, act);
    }

    void
    subtractionTest(ScamValue a, ScamValue b, ScamValue exp = makeNothing())
    {
        ExtendedNumeric extA(a);
        ExtendedNumeric extB(b);
        ExtendedNumeric extC { extA - extB };
        ScamValue act = extC.get();

        check4(a, b, exp, act);
    }

    void
    multiplicationTest(ScamValue a, ScamValue b, ScamValue exp = makeNothing())
    {
        ExtendedNumeric extA(a);
        ExtendedNumeric extB(b);
        ExtendedNumeric extC { extA * extB };
        ScamValue act = extC.get();

        check4(a, b, exp, act);
    }

    void divisionTest(ScamValue a, ScamValue b, ScamValue exp = makeNothing())
    {
        ExtendedNumeric extA(a);
        ExtendedNumeric extB(b);
        ExtendedNumeric extC { extA / extB };
        ScamValue act = extC.get();

        check4(a, b, exp, act);
    }

    void modulusTest(ScamValue a, ScamValue b, ScamValue exp = makeNothing())
    {
        ExtendedNumeric extA(a);
        ExtendedNumeric extB(b);
        ExtendedNumeric extC { extA % extB };
        ScamValue act = extC.get();

        check4(a, b, exp, act);
    }
};

TEST_F(ExtendedNumericTest, CreateNonNumeric)
{
    EXPECT_THROW(new ExtendedNumeric(makeNull()), ScamException);
}

TEST_F(ExtendedNumericTest, CreateCompareTest)
{
    createAndCompareTest(nanValue);
    createAndCompareTest(negInfValue);
    createAndCompareTest(posInfValue);
    createAndCompareTest(intValue1);
    createAndCompareTest(rationalValue1);
    createAndCompareTest(realValue1);
    createAndCompareTest(complexValue1);
}

TEST_F(ExtendedNumericTest, CompareALessB_NaN)
{
    compareALessBTest(nanValue, nanValue);

    compareALessBTest(nanValue, intValue1);
    compareALessBTest(intValue1, nanValue);

    compareALessBTest(nanValue, negInfValue);
    compareALessBTest(nanValue, posInfValue);
    compareALessBTest(nanValue, complexValue1);
    compareALessBTest(nanValue, realValue1);
    compareALessBTest(nanValue, rationalValue1);
}

TEST_F(ExtendedNumericTest, CompareALessB)
{
    compareALessBTest(negInfValue, intValue1);
    compareALessBTest(negInfValue, rationalValue1);
    compareALessBTest(negInfValue, realValue1);
    compareALessBTest(negInfValue, posInfValue);

    compareALessBTest(intValue1,      intValue2);
    compareALessBTest(rationalValue1, rationalValue2);
    compareALessBTest(realValue1,     realValue2);

    compareALessBTest(rationalValue1, intValue1);
    compareALessBTest(realValue1,     intValue1);

    compareALessBTest(rationalValue1, realValue1);

    compareALessBTest(intValue1,      posInfValue);
    compareALessBTest(rationalValue1, posInfValue);
    compareALessBTest(realValue1,     posInfValue);
}

TEST_F(ExtendedNumericTest, Addition_NaN)
{
    additionTest(nanValue, nanValue);

    additionTest(nanValue, intValue1);
    additionTest(intValue1, nanValue);

    additionTest(nanValue, negInfValue);
    additionTest(nanValue, posInfValue);
    additionTest(nanValue, complexValue1);
    additionTest(nanValue, realValue1);
    additionTest(nanValue, rationalValue1);
}

TEST_F(ExtendedNumericTest, Addition)
{
    additionTest(negInfValue, intValue1,      negInfValue);
    additionTest(negInfValue, rationalValue1, negInfValue);
    additionTest(negInfValue, realValue1,     negInfValue);
    additionTest(negInfValue, posInfValue,    nanValue);

    additionTest(intValue1,      intValue2,      makeInteger(7, true));
    additionTest(rationalValue1, rationalValue2, makeRational(5, 4, true));
    additionTest(realValue1,     realValue2,     makeReal(6.2, true));

    additionTest(rationalValue1, intValue1,     makeRational(5, 2, true));
    additionTest(realValue1,     intValue1,     makeReal(3.2, true));

    additionTest(rationalValue1, realValue1,   makeReal(1.7, true));

    additionTest(intValue1,      posInfValue, posInfValue);
    additionTest(rationalValue1, posInfValue, posInfValue);
    additionTest(realValue1,     posInfValue, posInfValue);
}

TEST_F(ExtendedNumericTest, Subtraction_NaN)
{
    subtractionTest(nanValue, nanValue);

    subtractionTest(nanValue, intValue1);
    subtractionTest(intValue1, nanValue);

    subtractionTest(nanValue, negInfValue);
    subtractionTest(nanValue, posInfValue);
    subtractionTest(nanValue, complexValue1);
    subtractionTest(nanValue, realValue1);
    subtractionTest(nanValue, rationalValue1);
}

TEST_F(ExtendedNumericTest, Subtraction)
{
    subtractionTest(negInfValue, intValue1,      negInfValue);
    subtractionTest(negInfValue, rationalValue1, negInfValue);
    subtractionTest(negInfValue, realValue1,     negInfValue);
    subtractionTest(negInfValue, posInfValue,    negInfValue);

    subtractionTest(intValue1,      negInfValue, posInfValue);
    subtractionTest(rationalValue1, negInfValue, posInfValue);
    subtractionTest(realValue1,     negInfValue, posInfValue);

    subtractionTest(intValue2,      intValue1,      makeInteger(3, true));
    subtractionTest(rationalValue2, rationalValue1, makeRational(1, 4, true));
    subtractionTest(realValue2,     realValue1,     makeReal(3.8, true));

    subtractionTest(rationalValue1, intValue1,     makeRational(-3, 2, true));
    subtractionTest(realValue1,     intValue1,     makeReal(-0.8, true));

    subtractionTest(rationalValue1, realValue1,   makeReal(-0.7, true));

    subtractionTest(posInfValue, intValue1,      posInfValue);
    subtractionTest(posInfValue, rationalValue1, posInfValue);
    subtractionTest(posInfValue, realValue1,     posInfValue);
    subtractionTest(posInfValue, negInfValue,    posInfValue);

    subtractionTest(intValue1,      posInfValue, negInfValue);
    subtractionTest(rationalValue1, posInfValue, negInfValue);
    subtractionTest(realValue1,     posInfValue, negInfValue);
    subtractionTest(negInfValue,    posInfValue, negInfValue);
}

TEST_F(ExtendedNumericTest, Multiplication_NaN)
{
    multiplicationTest(nanValue, nanValue);

    multiplicationTest(nanValue, intValue1);
    multiplicationTest(intValue1, nanValue);

    multiplicationTest(nanValue, negInfValue);
    multiplicationTest(nanValue, posInfValue);
    multiplicationTest(nanValue, complexValue1);
    multiplicationTest(nanValue, realValue1);
    multiplicationTest(nanValue, rationalValue1);
}

TEST_F(ExtendedNumericTest, Multiplication)
{
    multiplicationTest(negInfValue, intValue1,      negInfValue);
    multiplicationTest(negInfValue, rationalValue1, negInfValue);
    multiplicationTest(negInfValue, realValue1,     negInfValue);
    multiplicationTest(negInfValue, posInfValue,    negInfValue);

    multiplicationTest(intValue2,      intValue1,    makeInteger(10, true));
    multiplicationTest(rationalValue2, rationalValue1,
                       makeRational(3, 8, true));
    multiplicationTest(realValue2,     realValue1,   makeReal(6.0, true));

    multiplicationTest(rationalValue1, intValue1,    makeInteger(1, true));
    multiplicationTest(realValue1,     intValue1,    makeReal(2.4, true));

    multiplicationTest(rationalValue1, realValue1,   makeReal(0.6, true));

    multiplicationTest(intValue1,      posInfValue, posInfValue);
    multiplicationTest(rationalValue1, posInfValue, posInfValue);
    multiplicationTest(realValue1,     posInfValue, posInfValue);
}

TEST_F(ExtendedNumericTest, Division_NaN)
{
    divisionTest(negInfValue, posInfValue,    nanValue);

    divisionTest(intValue1,      negInfValue, nanValue);
    divisionTest(rationalValue1, negInfValue, nanValue);
    divisionTest(realValue1,     negInfValue, nanValue);

    divisionTest(nanValue, nanValue);

    divisionTest(nanValue, intValue1);
    divisionTest(intValue1, nanValue);

    divisionTest(nanValue, negInfValue);
    divisionTest(nanValue, posInfValue);
    divisionTest(nanValue, complexValue1);
    divisionTest(nanValue, realValue1);
    divisionTest(nanValue, rationalValue1);


    divisionTest(posInfValue, negInfValue,    nanValue);

    divisionTest(intValue1,      posInfValue, nanValue);
    divisionTest(rationalValue1, posInfValue, nanValue);
    divisionTest(realValue1,     posInfValue, nanValue);
}

TEST_F(ExtendedNumericTest, Division)
{
    divisionTest(negInfValue, intValue1,      negInfValue);
    divisionTest(negInfValue, rationalValue1, negInfValue);
    divisionTest(negInfValue, realValue1,     negInfValue);
    divisionTest(intValue2,      intValue1,      makeRational(5, 2, true));
    divisionTest(rationalValue2, rationalValue1, makeRational(3, 2, true));
    divisionTest(realValue2,     realValue1,     makeReal(5 / 1.2, true));

    divisionTest(rationalValue1, intValue1,     makeRational(1, 4, true));
    divisionTest(realValue1,     intValue1,     makeReal(0.6, true));

    divisionTest(rationalValue1, realValue1,   makeRational(5, 12, true));

    divisionTest(posInfValue, intValue1,      posInfValue);
    divisionTest(posInfValue, rationalValue1, posInfValue);
    divisionTest(posInfValue, realValue1,     posInfValue);
}

TEST_F(ExtendedNumericTest, Modulus_NaN)
{
    modulusTest(negInfValue, intValue1,      nanValue);
    modulusTest(negInfValue, rationalValue1, nanValue);
    modulusTest(negInfValue, realValue1,     nanValue);
    modulusTest(negInfValue, posInfValue,    nanValue);

    modulusTest(intValue1,      negInfValue, nanValue);
    modulusTest(rationalValue1, negInfValue, nanValue);
    modulusTest(realValue1,     negInfValue, nanValue);

    modulusTest(nanValue, nanValue);

    modulusTest(nanValue, intValue1);
    modulusTest(intValue1, nanValue);

    modulusTest(nanValue, negInfValue);
    modulusTest(nanValue, posInfValue);
    modulusTest(nanValue, complexValue1);
    modulusTest(nanValue, realValue1);
    modulusTest(nanValue, rationalValue1);

    modulusTest(posInfValue, intValue1,      nanValue);
    modulusTest(posInfValue, rationalValue1, nanValue);
    modulusTest(posInfValue, realValue1,     nanValue);
    modulusTest(posInfValue, negInfValue,    nanValue);

    modulusTest(intValue1,      posInfValue, nanValue);
    modulusTest(rationalValue1, posInfValue, nanValue);
    modulusTest(realValue1,     posInfValue, nanValue);
}

TEST_F(ExtendedNumericTest, Modulus)
{
    modulusTest(intValue2,      intValue1,      makeInteger(1, true));
    modulusTest(rationalValue2, rationalValue1, makeRational(1, 4, true));
    modulusTest(realValue2,     realValue1,     makeReal(0.2, true));

    modulusTest(rationalValue1, intValue1,     makeRational(1, 2, true));
    modulusTest(realValue1,     intValue1,     makeReal(1.2, true));

    modulusTest(rationalValue1, realValue1,   makeReal(0.5, true));

    modulusTest(makeInteger("100000000000000000000000000000000000000003", true),
		makeInteger(5, true),
		makeInteger(3, true));
}

