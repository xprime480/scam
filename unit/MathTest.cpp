#include "TestBase.hpp"

using namespace std;
using namespace scam;

class MathTest : public TestBase
{
protected:
    MathTest()
        : TestBase(false)
    {
    }
};

TEST_F(MathTest, AddZeroArgs)
{
    ScamValue expr = parseAndEvaluate("(+)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, AddOneArg)
{
    ScamValue expr = parseAndEvaluate("(+ 2)");
    expectInteger(expr, 2, "2", true);
}

TEST_F(MathTest, AddTwoArgs)
{
    ScamValue expr = parseAndEvaluate("(+ 2 2)");
    expectInteger(expr, 4, "4", true);
}

TEST_F(MathTest, AddManyArgs)
{
    ScamValue expr = parseAndEvaluate("(+ 2 2 -1 -3 4)");
    expectInteger(expr, 4, "4", true);
}

TEST_F(MathTest, AddRationals)
{
    ScamValue expr = parseAndEvaluate("(+ 1/3 1/3)");
    expectRational(expr, pair<int, int>(2,3), "2/3", true);
}

TEST_F(MathTest, AddTypeUnification)
{
    ScamValue expr = parseAndEvaluate("(+ 2 2.5)");
    expectRational(expr, pair<int, int>(9,2), "9/2", false);
}

TEST_F(MathTest, AddSimplificationRealToInt)
{
    ScamValue expr = parseAndEvaluate("(+ 2.5 2.5)");
    expectInteger(expr, 5, "5", false);
}

TEST_F(MathTest, AddComplex)
{
    ScamValue expr = parseAndEvaluate("(+ 1+I 1+I)");
    ScamValue null = ExpressionFactory::makeNull();
    expectComplex(expr, null, null, "2+2i", true);
}

TEST_F(MathTest, AddNegInf)
{
    ScamValue expr = parseAndEvaluate("(+ 2 -inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, AddPosInf)
{
    ScamValue expr = parseAndEvaluate("(+ 2 +inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, AddNaN)
{
    ScamValue expr = parseAndEvaluate("(+ 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, AddOppositeInf)
{
    ScamValue expr = parseAndEvaluate("(+ -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, AddSameInf)
{
    ScamValue expr = parseAndEvaluate("(+ +inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, AddBadArgument)
{
    ScamValue expr = parseAndEvaluate("(+ 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, SubZeroArgs)
{
    ScamValue expr = parseAndEvaluate("(-)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, SubOneArg)
{
    ScamValue expr = parseAndEvaluate("(- 2)");
    expectInteger(expr, -2, "-2", true);
}

TEST_F(MathTest, SubTwoArgs)
{
    ScamValue expr = parseAndEvaluate("(- 2 2)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, SubManyArgs)
{
    ScamValue expr = parseAndEvaluate("(- 2 2 -1 -3 4)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, SubRationals)
{
    ScamValue expr = parseAndEvaluate("(- 7/12 5/12)");
    expectRational(expr, pair<int, int>(1,6), "1/6", true);
}

TEST_F(MathTest, SubTypeUnification)
{
    ScamValue expr = parseAndEvaluate("(- 2 1.5)");
    expectRational(expr, pair<int,int>(1,2), "1/2", false);
}

TEST_F(MathTest, SubSimplificationRealToInt)
{
    ScamValue expr = parseAndEvaluate("(- 12.5 2.5)");
    expectInteger(expr, 10, "10", false);
}

TEST_F(MathTest, SubComplex)
{
    ScamValue expr = parseAndEvaluate("(- 1+i 2-3i)");
    ScamValue null = ExpressionFactory::makeNull();
    expectComplex(expr, null, null, "-1+4i", true);
}

TEST_F(MathTest, SubNegInf)
{
    ScamValue expr = parseAndEvaluate("(- 2 -inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, SubPosInf)
{
    ScamValue expr = parseAndEvaluate("(- 2 +inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, SubNaN)
{
    ScamValue expr = parseAndEvaluate("(- 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, SubOppositeInf)
{
    ScamValue expr = parseAndEvaluate("(- -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, SubSameInf)
{
    ScamValue expr = parseAndEvaluate("(- +inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, SubBadArgument)
{
    ScamValue expr = parseAndEvaluate("(- 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, MulZeroArgs)
{
    ScamValue expr = parseAndEvaluate("(*)");
    expectInteger(expr, 1, "1", true);
}

TEST_F(MathTest, MulOneArg)
{
    ScamValue expr = parseAndEvaluate("(* 2)");
    expectInteger(expr, 2, "2", true);
}

TEST_F(MathTest, MulTwoArgs)
{
    ScamValue expr = parseAndEvaluate("(* 2 3)");
    expectInteger(expr, 6, "6", true);
}

TEST_F(MathTest, MulManyArgs)
{
    ScamValue expr = parseAndEvaluate("(* 2 2 -1 4)");
    expectInteger(expr, -16, "-16", true);
}

TEST_F(MathTest, MulRationals)
{
    ScamValue expr = parseAndEvaluate("(*  3/10 -2/3)");
    expectRational(expr, pair<int, int>(-1,5), "-1/5", true);
}

TEST_F(MathTest, MulTypeUnification)
{
    ScamValue expr = parseAndEvaluate("(* 2 2.125)");
    expectRational(expr, pair<int,int>(17,4), "17/4", false);
}

TEST_F(MathTest, MulSimplificationRealToInt)
{
    ScamValue expr = parseAndEvaluate("(* #e0.125 8)");
    expectInteger(expr, 1, "1", true);
}

TEST_F(MathTest, MulComplex)
{
    ScamValue expr = parseAndEvaluate("(* 1+i 2-3i)");
    ScamValue null = ExpressionFactory::makeNull();
    expectComplex(expr, null, null, "5-i", true);
}

TEST_F(MathTest, MulComplexNaN)
{
    ScamValue expr = parseAndEvaluate("(* 1+i 2-nan.0i)");
    ScamValue null = ExpressionFactory::makeNull();
    expectComplex(expr, null, null, "+nan.0+nan.0i", false);
}

TEST_F(MathTest, MulComplexInf)
{
    ScamValue expr = parseAndEvaluate("(* 1+inf.0i 1-inf.0i)");
    ScamValue null = ExpressionFactory::makeNull();
    expectComplex(expr, null, null, "+inf.0+nan.0i", false);
}

TEST_F(MathTest, MulNegInf)
{
    ScamValue expr = parseAndEvaluate("(* 2 -inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, MulPosInf)
{
    ScamValue expr = parseAndEvaluate("(* 2 +inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, MulNegRealPosInf)
{
    ScamValue expr = parseAndEvaluate("(* -2.0123 +inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, MulNaN)
{
    ScamValue expr = parseAndEvaluate("(* 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, MulOppositeInf)
{
    ScamValue expr = parseAndEvaluate("(* -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, MulSameInf)
{
    ScamValue expr = parseAndEvaluate("(* +inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, MulBadArgument)
{
    ScamValue expr = parseAndEvaluate("(* 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, DivZeroArgs)
{
    ScamValue expr = parseAndEvaluate("(/)");
    expectInteger(expr, 1, "1", true);
}

TEST_F(MathTest, DivOneArg)
{
    ScamValue expr = parseAndEvaluate("(/ 2)");
    expectRational(expr, pair<int,int>(1,2), "1/2", true);
}

TEST_F(MathTest, DivTwoArgs)
{
    ScamValue expr = parseAndEvaluate("(/ 2 5)");
    expectRational(expr, pair<int,int>(2,5), "2/5", true);
}

TEST_F(MathTest, DivManyArgs)
{
    ScamValue expr = parseAndEvaluate("(/ 1 2 2 2)");
    expectRational(expr, pair<int,int>(1,8), "1/8", true);
}

TEST_F(MathTest, DivRationals)
{
    ScamValue expr = parseAndEvaluate("(/ 3/10 11/7)");
    expectRational(expr, pair<int, int>(21,110), "21/110", true);
}

TEST_F(MathTest, DivTypeUnification)
{
    ScamValue expr = parseAndEvaluate("(/ 2.5 1)");
    expectRational(expr, pair<int,int>(5,2), "5/2", false);
}

TEST_F(MathTest, DivComplex)
{
    ScamValue expr = parseAndEvaluate("(/ 1+i 1-i)");
    ScamValue null = ExpressionFactory::makeNull();
    expectComplex(expr, null, null, "+i", true);
}

TEST_F(MathTest, DivSimplificationRealToInt)
{
    ScamValue expr = parseAndEvaluate("(/ 12.5 2.5)");
    expectInteger(expr, 5, "5", false);
}

TEST_F(MathTest, DivZeroByInf)
{
    ScamValue expr = parseAndEvaluate("(/ 0 -inf.0)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, DivRealByInf)
{
    ScamValue expr = parseAndEvaluate("(/ 2 -inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivInfByReal)
{
    ScamValue expr = parseAndEvaluate("(/ +inf.0 2)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, DivPosInfByNegReal)
{
    ScamValue expr = parseAndEvaluate("(/ +inf.0 -2.0123)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, DivNaN)
{
    ScamValue expr = parseAndEvaluate("(/ +nan.0 2)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivByNaN)
{
    ScamValue expr = parseAndEvaluate("(/ 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivOppositeInf)
{
    ScamValue expr = parseAndEvaluate("(/ -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivSameInf)
{
    ScamValue expr = parseAndEvaluate("(/ -inf.0 -inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivBadArgument)
{
    ScamValue expr = parseAndEvaluate("(/ 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, DivByZero)
{
    ScamValue expr = parseAndEvaluate("(/ 2 0)");
    expectError(expr);
}

TEST_F(MathTest, DivZeroBy)
{
    ScamValue expr = parseAndEvaluate("(/ 0 2)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModZero)
{
    ScamValue expr = parseAndEvaluate("(%)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModOne)
{
    ScamValue expr = parseAndEvaluate("(% 5)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModTwo)
{
    ScamValue expr = parseAndEvaluate("(% 5 2)");
    expectInteger(expr, 1, "1", true);
}

TEST_F(MathTest, ModThreeIgnoresExtra)
{
    ScamValue expr = parseAndEvaluate("(% 5 2 0 0 0)");
    expectInteger(expr, 1, "1", true);
}

TEST_F(MathTest, ModSimplificationRealToInt)
{
    ScamValue expr = parseAndEvaluate("(% 12.5 2.5)");
    expectInteger(expr, 0, "0", false);
}

TEST_F(MathTest, ModZeroByInf)
{
    ScamValue expr = parseAndEvaluate("(% 0 -inf.0)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModRealByInf)
{
    ScamValue expr = parseAndEvaluate("(% 2 -inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModInfByReal)
{
    ScamValue expr = parseAndEvaluate("(% +inf.0 2)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModPosInfByNegReal)
{
    ScamValue expr = parseAndEvaluate("(% +inf.0 -2.0123)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModNaN)
{
    ScamValue expr = parseAndEvaluate("(% +nan.0 2)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModByNaN)
{
    ScamValue expr = parseAndEvaluate("(% 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModOppositeInf)
{
    ScamValue expr = parseAndEvaluate("(% -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModSameInf)
{
    ScamValue expr = parseAndEvaluate("(% -inf.0 -inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}


TEST_F(MathTest, ModByZero)
{
    ScamValue expr = parseAndEvaluate("(% 5 0)");
    expectError(expr, "Modulus By Zero");
}

TEST_F(MathTest, ModZeroBy)
{
    ScamValue expr = parseAndEvaluate("(% 0 5)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModRational)
{
    ScamValue expr = parseAndEvaluate("(% 3/4 1/10)");
    expectRational(expr, pair<int, int>(1,20), "1/20", true);
}

TEST_F(MathTest, ModReal)
{
    ScamValue expr = parseAndEvaluate("(% 0.75 0.5)");
    expectRational(expr, pair<int,int>(1,4), "1/4", false);
}

TEST_F(MathTest, Nested)
{
    ScamValue expr = parseAndEvaluate("(+ (* 2 3) (/ 1 5) (- 3))");
    expectRational(expr, pair<int,int>(16,5), "16/5", true);
}

TEST_F(MathTest, NestedWithError)
{
    ScamValue expr = parseAndEvaluate("(+ (* 2 3) (/ 1 (+ 5 -5)) (- 3))");
    expectError(expr);
}

TEST_F(MathTest, NestedWithNaN)
{
    ScamValue expr = parseAndEvaluate("(+ (* 2 3) (/ 1 (+ +nan.0 -5)) (- 3))");
    expectSpecialNumeric(expr, "+nan.0");
}
