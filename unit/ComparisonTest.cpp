
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class ComparisonTest : public ExpressionTestBase
{
};

TEST_F(ComparisonTest, EqZeroForms)
{
    ExprHandle expr = parseAndEvaluate("(=)");
    expectBoolean(expr, true, "#t");
}

TEST_F(ComparisonTest, EqOneNumber)
{
    ExprHandle expr = parseAndEvaluate("(= 3)");
    expectBoolean(expr, true, "#t");
}

TEST_F(ComparisonTest, EqTwoNumbersTrue)
{
    ExprHandle expr = parseAndEvaluate("(= 3 3)");
    expectBoolean(expr, true, "#t");
}

TEST_F(ComparisonTest, EqTwoNumbersFalse)
{
    ExprHandle expr = parseAndEvaluate("(= 3 5)");
    expectBoolean(expr, false, "#f");
}

TEST_F(ComparisonTest, EqManyNumbersTrue)
{
    ExprHandle expr = parseAndEvaluate("(= 3 3 3 3 3)");
    expectBoolean(expr, true, "#t");
}

TEST_F(ComparisonTest, EqManyNumbersFalse)
{
    ExprHandle expr = parseAndEvaluate("(= 3 3 3 3 3 5)");
    expectBoolean(expr, false, "#f");
}

TEST_F(ComparisonTest, EqOneString)
{
    ExprHandle expr = parseAndEvaluate("(= \"x\")");
    expectBoolean(expr, true, "#t");
}

TEST_F(ComparisonTest, EqTwoStringsTrue)
{
    ExprHandle expr = parseAndEvaluate("(= \"x\" \"x\")");
    expectBoolean(expr, true, "#t");
}

TEST_F(ComparisonTest, EqTwoStringsFalse)
{
    ExprHandle expr = parseAndEvaluate("(= \"x\" \"z\")");
    expectBoolean(expr, false, "#f");
}

TEST_F(ComparisonTest, EqManyStringsTrue)
{
    ExprHandle expr = parseAndEvaluate("(= \"x\" \"x\" \"x\" \"x\" \"x\")");
    expectBoolean(expr, true, "#t");
}

TEST_F(ComparisonTest, EqManyStringsFalse)
{
    ExprHandle expr = parseAndEvaluate("(= \"x\" \"x\" \"x\" \"x\" \"z\")");
    expectBoolean(expr, false, "#f");
}
