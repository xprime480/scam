
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class ComparisonTest : public ExpressionTestBase
{
protected:
    void expectTrue(string const & input)
    {
        ExprHandle expr = parseAndEvaluate(input);
        expectBoolean(expr, true, "#t");
    }

    void expectFalse(string const & input)
    {
        ExprHandle expr = parseAndEvaluate(input);
        expectBoolean(expr, false, "#f");
    }
};

TEST_F(ComparisonTest, EqZeroForms)
{
    expectTrue("(=)");
}

TEST_F(ComparisonTest, EqNumber)
{
    expectTrue("(= 3)");
    expectTrue("(= 3 3)");
    expectFalse("(= 3 6)");
    expectTrue("(= 3 3 3 3 3 3)");
    expectFalse("(= 3 3 3 3 3 5)");
}

TEST_F(ComparisonTest, EqString)
{
    expectTrue("(= \"A\")");
    expectTrue("(= \"A\" \"A\")");
    expectFalse("(= \"A\" \"Z\")");
    expectTrue("(= \"A\" \"A\" \"A\" \"A\" \"A\" \"A\")");
    expectFalse("(= \"A\" \"A\" \"A\" \"A\" \"A\" \"Z\")");
}

TEST_F(ComparisonTest, EqBadArgs)
{
    ExprHandle expr = parseAndEvaluate("(= #t)");
    expectError(expr);

    expr = parseAndEvaluate("(= 3 \"x\")");
    expectError(expr);
}

