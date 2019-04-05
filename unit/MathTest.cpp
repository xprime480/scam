
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class MathTest : public ExpressionTestBase
{
};

TEST_F(MathTest, AddZeroArgs)
{
    ScamExpr * expr = parseAndEvaluate("(+)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, AddOneArg)
{
    ScamExpr * expr = parseAndEvaluate("(+ 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(MathTest, AddTwoArgs)
{
    ScamExpr * expr = parseAndEvaluate("(+ 2 2)");
    expectInteger(expr, 4, "4");
}

TEST_F(MathTest, AddManyArgs)
{
    ScamExpr * expr = parseAndEvaluate("(+ 2 2 -1 -3 4)");
    expectInteger(expr, 4, "4");
}

TEST_F(MathTest, AddTypeUnification)
{
    ScamExpr * expr = parseAndEvaluate("(+ 2 2.5)");
    expectFloat(expr, 4.5, "4.5");
}

TEST_F(MathTest, AddBadArgument)
{
    ScamExpr * expr = parseAndEvaluate("(+ 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, SubZeroArgs)
{
    ScamExpr * expr = parseAndEvaluate("(-)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, SubOneArg)
{
    ScamExpr * expr = parseAndEvaluate("(- 2)");
    expectInteger(expr, -2, "-2");
}

TEST_F(MathTest, SubTwoArgs)
{
    ScamExpr * expr = parseAndEvaluate("(- 2 2)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, SubManyArgs)
{
    ScamExpr * expr = parseAndEvaluate("(- 2 2 -1 -3 4)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, SubTypeUnification)
{
    ScamExpr * expr = parseAndEvaluate("(- 2 1.5)");
    expectFloat(expr, 0.5, "0.5");
}

TEST_F(MathTest, SubBadArgument)
{
    ScamExpr * expr = parseAndEvaluate("(- 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, MulZeroArgs)
{
    ScamExpr * expr = parseAndEvaluate("(*)");
    expectInteger(expr, 1, "1");
}

TEST_F(MathTest, MulOneArg)
{
    ScamExpr * expr = parseAndEvaluate("(* 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(MathTest, MulTwoArgs)
{
    ScamExpr * expr = parseAndEvaluate("(* 2 3)");
    expectInteger(expr, 6, "6");
}

TEST_F(MathTest, MulManyArgs)
{
    ScamExpr * expr = parseAndEvaluate("(* 2 2 -1 4)");
    expectInteger(expr, -16, "-16");
}

TEST_F(MathTest, MulTypeUnification)
{
    ScamExpr * expr = parseAndEvaluate("(* 2 2.125)");
    expectFloat(expr, 4.25, "4.25");
}

TEST_F(MathTest, MulBadArgument)
{
    ScamExpr * expr = parseAndEvaluate("(* 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, DivZeroArgs)
{
    ScamExpr * expr = parseAndEvaluate("(/)");
    expectFloat(expr, 1, "1");
}

TEST_F(MathTest, DivOneArg)
{
    ScamExpr * expr = parseAndEvaluate("(/ 2)");
    expectFloat(expr, 0.5, "0.5");
}

TEST_F(MathTest, DivTwoArgs)
{
    ScamExpr * expr = parseAndEvaluate("(/ 2 5)");
    expectFloat(expr, 0.4, "0.4");
}

TEST_F(MathTest, DivManyArgs)
{
    ScamExpr * expr = parseAndEvaluate("(/ 1 2 2 2)");
    expectFloat(expr, 0.125, "0.125");
}

TEST_F(MathTest, DivTypeUnification)
{
    ScamExpr * expr = parseAndEvaluate("(/ 2.0 1)");
    expectFloat(expr, 2, "2");
}

TEST_F(MathTest, DivBadArgument)
{
    ScamExpr * expr = parseAndEvaluate("(/ 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, DivByZero)
{
    ScamExpr * expr = parseAndEvaluate("(/ 2 0)");
    expectError(expr);
}

TEST_F(MathTest, DivZeroBy)
{
    ScamExpr * expr = parseAndEvaluate("(/ 0 2)");
    expectFloat(expr, 0, "0");
}

TEST_F(MathTest, ModZero)
{
    ScamExpr * expr = parseAndEvaluate("(%)");
    expectFloat(expr, 0, "0");
}

TEST_F(MathTest, ModOne)
{
    ScamExpr * expr = parseAndEvaluate("(% 5)");
    expectFloat(expr, 0, "0");
}

TEST_F(MathTest, ModTwo)
{
    ScamExpr * expr = parseAndEvaluate("(% 5 2)");
    expectFloat(expr, 1, "1");
}

TEST_F(MathTest, ModThreeIgnoresExtra)
{
    ScamExpr * expr = parseAndEvaluate("(% 5 2 0 0 0)");
    expectFloat(expr, 1, "1");
}

TEST_F(MathTest, ModByZero)
{
    ScamExpr * expr = parseAndEvaluate("(% 5 0)");
    expectError(expr, "Modulus By Zero");
}

TEST_F(MathTest, ModZeroBy)
{
    ScamExpr * expr = parseAndEvaluate("(% 0 5)");
    expectFloat(expr, 0, "0");
}

TEST_F(MathTest, Nested)
{
    ScamExpr * expr = parseAndEvaluate("(+ (* 2 3) (/ 1 5) (- 3))");
    expectFloat(expr, 3.2, "3.2");
}

TEST_F(MathTest, NestedWithError)
{
    ScamExpr * expr = parseAndEvaluate("(+ (* 2 3) (/ 1 (+ 5 -5)) (- 3))");
    expectError(expr);
}
