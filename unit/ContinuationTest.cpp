#include "TestBase.hpp"

#include "util/DebugTrace.hpp"

using namespace std;
using namespace scam;

class ContinuationTest : public TestBase
{
};

TEST_F(ContinuationTest, UnusedCont)
{
    ExprHandle expr = parseAndEvaluate("(+ 4 (call/cc (lambda (k) 2)))");
    expectInteger(expr, 6, "6", true);
}

TEST_F(ContinuationTest, TrivialCont)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/callcc/trivial.scm");
    expectInteger(expr, 5, "5", true);
}

TEST_F(ContinuationTest, PersistedCont)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/callcc/persisted.scm");
    expectInteger(expr, 1, "1", true);
}

TEST_F(ContinuationTest, NonLambdaArg)
{
    ExprHandle expr = parseAndEvaluate("(call/cc 2)");
    expectError(expr);
}

TEST_F(ContinuationTest, InvokedWithNoArgs)
{
    ExprHandle expr = parseAndEvaluate("(+ 4 (call/cc (lambda (k) (k))))");
    expectError(expr);
}

TEST_F(ContinuationTest, InvokedWithExtraArgs)
{
    ExprHandle expr =
        parseAndEvaluate("(+ 4 (call/cc (lambda (k) (k 1 2))))");
    expectError(expr);
}
