
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class MathTest : public ExpressionTestBase
{
};

TEST_F(MathTest, AddZeroArgs)
{
    ExprHandle expr = parseAndEvaluate("(+)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, AddOneArg)
{
    ExprHandle expr = parseAndEvaluate("(+ 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(MathTest, AddTwoArgs)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 2)");
    expectInteger(expr, 4, "4");
}

TEST_F(MathTest, AddManyArgs)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 2 -1 -3 4)");
    expectInteger(expr, 4, "4");
}

TEST_F(MathTest, AddTypeUnification)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 2.5)");
    expectFloat(expr, 4.5, "4.5");
}

TEST_F(MathTest, AddBadArgument)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, SubZeroArgs)
{
    ExprHandle expr = parseAndEvaluate("(-)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, SubOneArg)
{
    ExprHandle expr = parseAndEvaluate("(- 2)");
    expectInteger(expr, -2, "-2");
}

TEST_F(MathTest, SubTwoArgs)
{
    ExprHandle expr = parseAndEvaluate("(- 2 2)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, SubManyArgs)
{
    ExprHandle expr = parseAndEvaluate("(- 2 2 -1 -3 4)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, SubTypeUnification)
{
    ExprHandle expr = parseAndEvaluate("(- 2 1.5)");
    expectFloat(expr, 0.5, "0.5");
}

TEST_F(MathTest, SubBadArgument)
{
    ExprHandle expr = parseAndEvaluate("(- 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, MulZeroArgs)
{
    ExprHandle expr = parseAndEvaluate("(*)");
    expectInteger(expr, 1, "1");
}

TEST_F(MathTest, MulOneArg)
{
    ExprHandle expr = parseAndEvaluate("(* 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(MathTest, MulTwoArgs)
{
    ExprHandle expr = parseAndEvaluate("(* 2 3)");
    expectInteger(expr, 6, "6");
}

TEST_F(MathTest, MulManyArgs)
{
    ExprHandle expr = parseAndEvaluate("(* 2 2 -1 4)");
    expectInteger(expr, -16, "-16");
}

TEST_F(MathTest, MulTypeUnification)
{
    ExprHandle expr = parseAndEvaluate("(* 2 2.125)");
    expectFloat(expr, 4.25, "4.25");
}

TEST_F(MathTest, MulBadArgument)
{
    ExprHandle expr = parseAndEvaluate("(* 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, DivZeroArgs)
{
    ExprHandle expr = parseAndEvaluate("(/)");
    expectFloat(expr, 1, "1");
}

TEST_F(MathTest, DivOneArg)
{
    ExprHandle expr = parseAndEvaluate("(/ 2)");
    expectFloat(expr, 0.5, "0.5");
}

TEST_F(MathTest, DivTwoArgs)
{
    ExprHandle expr = parseAndEvaluate("(/ 2 5)");
    expectFloat(expr, 0.4, "0.4");
}

TEST_F(MathTest, DivManyArgs)
{
    ExprHandle expr = parseAndEvaluate("(/ 1 2 2 2)");
    expectFloat(expr, 0.125, "0.125");
}

TEST_F(MathTest, DivTypeUnification)
{
    ExprHandle expr = parseAndEvaluate("(/ 2.0 1)");
    expectFloat(expr, 2, "2");
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

TEST_F(MathTest, Nested)
{
    ExprHandle expr = parseAndEvaluate("(+ (* 2 3) (/ 1 5) (- 3))");
    expectFloat(expr, 3.2, "3.2");
}

TEST_F(MathTest, NestedWithError)
{
    ExprHandle expr = parseAndEvaluate("(+ (* 2 3) (/ 1 (+ 5 -5)) (- 3))");
    expectError(expr);
}
