
#include "ExpressionTestBase.hpp"

#include "ScamException.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace std;
using namespace scam;

class LogicTest : public ExpressionTestBase
{
};

TEST_F(LogicTest, IfTrue)
{
    ExprHandle expr = parseAndEvaluate("(if #t +1 -1)");
    expectInteger(expr, 1, "1");
}

TEST_F(LogicTest, IfFalse)
{
    ExprHandle expr = parseAndEvaluate("(if #f +1 -1)");
    expectInteger(expr, -1, "-1");
}

TEST_F(LogicTest, IfFalseNoElse)
{
    ExprHandle expr = parseAndEvaluate("(if #f +1)");
    expectNil(expr);
}

TEST_F(LogicTest, IfDoesntEvaluateUnusedClause)
{
    ExprHandle expr = parseAndEvaluate("(if #t (* 2 3) (/ 1 0))");
    expectInteger(expr, 6, "6");
}

TEST_F(LogicTest, IfTestError)
{
    ExprHandle expr = parseAndEvaluate("(if (/ 1 0) (* 2 3) (/ 1 0))");
    expectError(expr);
}

TEST_F(LogicTest, IfThenError)
{
    ExprHandle expr = parseAndEvaluate("(if \"strings are true\" (/ 1 0))");
    expectError(expr);
}

TEST_F(LogicTest, IfTooFewClauses)
{
    ExprHandle expr = parseAndEvaluate("(if #t)");
    expectError(expr);
}

TEST_F(LogicTest, IfTooManyClauses)
{
    ExprHandle expr = parseAndEvaluate("(if #t 1 2 3 4 5 ())");
    expectError(expr);
}
