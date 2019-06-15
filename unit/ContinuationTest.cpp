#include "TestBase.hpp"

#include "util/DebugTrace.hpp"

using namespace std;
using namespace scam;

class ContinuationTest : public TestBase
{
};

TEST_F(ContinuationTest, UnusedCont)
{
    ScamValue expr = readEval("(+ 4 (call/cc (lambda (k) 2)))");
    expectInteger(expr, 6, "6", true);
}

TEST_F(ContinuationTest, TrivialCont)
{
    ScamValue expr = readEvalFile("scripts/callcc/trivial.scm");
    expectInteger(expr, 5, "5", true);
}

TEST_F(ContinuationTest, PersistedCont)
{
    ScamValue expr = readEvalFile("scripts/callcc/persisted.scm");
    expectInteger(expr, 1, "1", true);
}

TEST_F(ContinuationTest, NonLambdaArg)
{
    ScamValue expr = readEval("(call/cc 2)");
    expectError(expr);
}

TEST_F(ContinuationTest, InvokedWithNoArgs)
{
    ScamValue expr = readEval("(+ 4 (call/cc (lambda (k) (k))))");
    expectError(expr);
}

TEST_F(ContinuationTest, InvokedWithExtraArgs)
{
    ScamValue expr = readEval("(+ 4 (call/cc (lambda (k) (k 1 2))))");
    expectError(expr);
}
