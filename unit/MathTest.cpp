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
    ExprHandle expr = parseAndEvaluate("(+)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, AddOneArg)
{
    ExprHandle expr = parseAndEvaluate("(+ 2)");
    expectInteger(expr, 2, "2", true);
}

TEST_F(MathTest, AddTwoArgs)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 2)");
    expectInteger(expr, 4, "4", true);
}

TEST_F(MathTest, AddManyArgs)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 2 -1 -3 4)");
    expectInteger(expr, 4, "4", true);
}

TEST_F(MathTest, AddTypeUnification)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 2.5)");
    expectReal(expr, 4.5, "4.5", false);
}

TEST_F(MathTest, AddNegInf)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 -inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, AddPosInf)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 +inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, AddNaN)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, AddOppositeInf)
{
    ExprHandle expr = parseAndEvaluate("(+ -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, AddSameInf)
{
    ExprHandle expr = parseAndEvaluate("(+ +inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, AddBadArgument)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, SubZeroArgs)
{
    ExprHandle expr = parseAndEvaluate("(-)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, SubOneArg)
{
    ExprHandle expr = parseAndEvaluate("(- 2)");
    expectInteger(expr, -2, "-2", true);
}

TEST_F(MathTest, SubTwoArgs)
{
    ExprHandle expr = parseAndEvaluate("(- 2 2)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, SubManyArgs)
{
    ExprHandle expr = parseAndEvaluate("(- 2 2 -1 -3 4)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, SubTypeUnification)
{
    ExprHandle expr = parseAndEvaluate("(- 2 1.5)");
    expectReal(expr, 0.5, "0.5", false);
}

TEST_F(MathTest, SubNegInf)
{
    ExprHandle expr = parseAndEvaluate("(- 2 -inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, SubPosInf)
{
    ExprHandle expr = parseAndEvaluate("(- 2 +inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, SubNaN)
{
    ExprHandle expr = parseAndEvaluate("(- 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, SubOppositeInf)
{
    ExprHandle expr = parseAndEvaluate("(- -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, SubSameInf)
{
    ExprHandle expr = parseAndEvaluate("(- +inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, SubBadArgument)
{
    ExprHandle expr = parseAndEvaluate("(- 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, MulZeroArgs)
{
    ExprHandle expr = parseAndEvaluate("(*)");
    expectInteger(expr, 1, "1", true);
}

TEST_F(MathTest, MulOneArg)
{
    ExprHandle expr = parseAndEvaluate("(* 2)");
    expectInteger(expr, 2, "2", true);
}

TEST_F(MathTest, MulTwoArgs)
{
    ExprHandle expr = parseAndEvaluate("(* 2 3)");
    expectInteger(expr, 6, "6", true);
}

TEST_F(MathTest, MulManyArgs)
{
    ExprHandle expr = parseAndEvaluate("(* 2 2 -1 4)");
    expectInteger(expr, -16, "-16", true);
}

TEST_F(MathTest, MulTypeUnification)
{
    ExprHandle expr = parseAndEvaluate("(* 2 2.125)");
    expectReal(expr, 4.25, "4.25", false);
}

TEST_F(MathTest, MulNegInf)
{
    ExprHandle expr = parseAndEvaluate("(* 2 -inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, MulPosInf)
{
    ExprHandle expr = parseAndEvaluate("(* 2 +inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, MulNegRealPosInf)
{
    ExprHandle expr = parseAndEvaluate("(* -2.0123 +inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, MulNaN)
{
    ExprHandle expr = parseAndEvaluate("(* 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, MulOppositeInf)
{
    ExprHandle expr = parseAndEvaluate("(* -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, MulSameInf)
{
    ExprHandle expr = parseAndEvaluate("(* +inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, MulBadArgument)
{
    ExprHandle expr = parseAndEvaluate("(* 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, DivZeroArgs)
{
    ExprHandle expr = parseAndEvaluate("(/)");
    expectInteger(expr, 1, "1", true);
}

TEST_F(MathTest, DivOneArg)
{
    ExprHandle expr = parseAndEvaluate("(/ 2)");
    expectReal(expr, 0.5, "0.5", true);
}

TEST_F(MathTest, DivTwoArgs)
{
    ExprHandle expr = parseAndEvaluate("(/ 2 5)");
    expectReal(expr, 0.4, "0.4", true);
}

TEST_F(MathTest, DivManyArgs)
{
    ExprHandle expr = parseAndEvaluate("(/ 1 2 2 2)");
    expectReal(expr, 0.125, "0.125", true);
}

TEST_F(MathTest, DivTypeUnification)
{
    ExprHandle expr = parseAndEvaluate("(/ 2.5 1)");
    expectReal(expr, 2.5, "2.5", false);
    EXPECT_FALSE(expr->isExact());
}

TEST_F(MathTest, DivZeroByInf)
{
    ExprHandle expr = parseAndEvaluate("(/ 0 -inf.0)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, DivRealByInf)
{
    ExprHandle expr = parseAndEvaluate("(/ 2 -inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivInfByReal)
{
    ExprHandle expr = parseAndEvaluate("(/ +inf.0 2)");
    expectSpecialNumeric(expr, "+inf.0");
}

TEST_F(MathTest, DivPosInfByNegReal)
{
    ExprHandle expr = parseAndEvaluate("(/ +inf.0 -2.0123)");
    expectSpecialNumeric(expr, "-inf.0");
}

TEST_F(MathTest, DivNaN)
{
    ExprHandle expr = parseAndEvaluate("(/ +nan.0 2)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivByNaN)
{
    ExprHandle expr = parseAndEvaluate("(/ 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivOppositeInf)
{
    ExprHandle expr = parseAndEvaluate("(/ -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivSameInf)
{
    ExprHandle expr = parseAndEvaluate("(/ -inf.0 -inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, DivBadArgument)
{
    ExprHandle expr = parseAndEvaluate("(/ 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, DivByZero)
{
    ExprHandle expr = parseAndEvaluate("(/ 2 0)");
    expectError(expr);
}

TEST_F(MathTest, DivZeroBy)
{
    ExprHandle expr = parseAndEvaluate("(/ 0 2)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModZero)
{
    ExprHandle expr = parseAndEvaluate("(%)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModOne)
{
    ExprHandle expr = parseAndEvaluate("(% 5)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModTwo)
{
    ExprHandle expr = parseAndEvaluate("(% 5 2)");
    expectInteger(expr, 1, "1", true);
}

TEST_F(MathTest, ModThreeIgnoresExtra)
{
    ExprHandle expr = parseAndEvaluate("(% 5 2 0 0 0)");
    expectInteger(expr, 1, "1", true);
}

TEST_F(MathTest, ModZeroByInf)
{
    ExprHandle expr = parseAndEvaluate("(% 0 -inf.0)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModRealByInf)
{
    ExprHandle expr = parseAndEvaluate("(% 2 -inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModInfByReal)
{
    ExprHandle expr = parseAndEvaluate("(% +inf.0 2)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModPosInfByNegReal)
{
    ExprHandle expr = parseAndEvaluate("(% +inf.0 -2.0123)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModNaN)
{
    ExprHandle expr = parseAndEvaluate("(% +nan.0 2)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModByNaN)
{
    ExprHandle expr = parseAndEvaluate("(% 2 +nan.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModOppositeInf)
{
    ExprHandle expr = parseAndEvaluate("(% -inf.0 +inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}

TEST_F(MathTest, ModSameInf)
{
    ExprHandle expr = parseAndEvaluate("(% -inf.0 -inf.0)");
    expectSpecialNumeric(expr, "+nan.0");
}


TEST_F(MathTest, ModByZero)
{
    ExprHandle expr = parseAndEvaluate("(% 5 0)");
    expectError(expr, "Modulus By Zero");
}

TEST_F(MathTest, ModZeroBy)
{
    ExprHandle expr = parseAndEvaluate("(% 0 5)");
    expectInteger(expr, 0, "0", true);
}

TEST_F(MathTest, ModReal)
{
    ExprHandle expr = parseAndEvaluate("(% 0.75 0.5)");
    expectReal(expr, 0.25, "0.25", false);
}

TEST_F(MathTest, Nested)
{
    ExprHandle expr = parseAndEvaluate("(+ (* 2 3) (/ 1 5) (- 3))");
    expectReal(expr, 3.2, "3.2", true);
}

TEST_F(MathTest, NestedWithError)
{
    ExprHandle expr = parseAndEvaluate("(+ (* 2 3) (/ 1 (+ 5 -5)) (- 3))");
    expectError(expr);
}

TEST_F(MathTest, NestedWithNaN)
{
    ExprHandle expr = parseAndEvaluate("(+ (* 2 3) (/ 1 (+ +nan.0 -5)) (- 3))");
    expectSpecialNumeric(expr, "+nan.0");
}
