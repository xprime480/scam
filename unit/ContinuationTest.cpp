
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class ContinuationTest : public ExpressionTestBase
{
};

TEST_F(ContinuationTest, UnusedCont)
{
    ScamExpr * expr = parseAndEvaluate("(+ 4 (call/cc (lambda (k) 2)))");
    expectInteger(expr, 6, "6");
}

TEST_F(ContinuationTest, TrivialCont)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/callcc/trivial.scm");
    expectInteger(expr, 5, "5");
}

TEST_F(ContinuationTest, PersistedCont)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/callcc/persisted.scm");
    expectInteger(expr, 1, "1");
}

TEST_F(ContinuationTest, NonLambdaArg)
{
    ScamExpr * expr = parseAndEvaluate("(call/cc 2)");
    expectError(expr);
}
