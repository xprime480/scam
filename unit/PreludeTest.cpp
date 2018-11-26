
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class PreludeTest : public ExpressionTestBase
{
};

TEST_F(PreludeTest, MaxIntegerTest)
{
    ExprHandle expr = parseAndEvaluate("(max 123 -123)");
    expectInteger(expr, 123, "123");
}

TEST_F(PreludeTest, MaxFloatTest)
{
    ExprHandle expr = parseAndEvaluate("(max 42.01 17.5)");
    expectFloat(expr, 42.01, "42.01");
}

TEST_F(PreludeTest, MaxMixedNumericTest)
{
    ExprHandle expr = parseAndEvaluate("(max -5 -9.999)");
    expectInteger(expr, -5, "-5");
}

TEST_F(PreludeTest, MinIntegerTest)
{
    ExprHandle expr = parseAndEvaluate("(min 123 -123)");
    expectInteger(expr, -123, "-123");
}

TEST_F(PreludeTest, MinFloatTest)
{
    ExprHandle expr = parseAndEvaluate("(min 42.01 17.5)");
    expectFloat(expr, 17.5, "17.5");
}

TEST_F(PreludeTest, MinMixedNumericTest)
{
    ExprHandle expr = parseAndEvaluate("(min -5 -9.999)");
    expectFloat(expr, -9.999, "-9.999");
}

TEST_F(PreludeTest, MapTest)
{
    ExprHandle expr = parseAndEvaluate("(map integer? (list 1 0.123 \"xx\"))");
    expectList(expr, "(#t #f #f)", 3);
}

TEST_F(PreludeTest, EvenTest)
{
    expectTrue("(even? -2)");
    expectTrue("(even? 0)");
    expectTrue("(even? 2)");
    expectTrue("(even? 12398234)");

    expectFalse("(even? -3)");
    expectFalse("(even? 3)");
    expectFalse("(even? 82383)");

    expectFalse("(even? 2.0)");
    expectFalse("(even? #t)");
    expectFalse("(even? \"Silly, strings don't have parity\")");
}

TEST_F(PreludeTest, OddTest)
{
    expectTrue("(odd? -3)");
    expectTrue("(odd? 3)");
    expectTrue("(odd? 82383)");

    expectFalse("(odd? -2)");
    expectFalse("(odd? 0)");
    expectFalse("(odd? 2)");
    expectFalse("(odd? 12398234)");

    expectFalse("(odd? 3.0)");
    expectFalse("(odd? #t)");
    expectFalse("(odd? \"Silly, strings don't have parity\")");
}

TEST_F(PreludeTest, RequireTest)
{
    expectTrue("(require #t)");

    ExprHandle expr = parseAndEvaluate("(require #f)");
    expectError(expr, "No more choices");
}
