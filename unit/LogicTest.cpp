
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

TEST_F(LogicTest, AndZeroForms)
{
    ExprHandle expr = parseAndEvaluate("(and)");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, AndOneTrue)
{
    ExprHandle expr = parseAndEvaluate("(and 3)");
    expectInteger(expr, 3, "3");
}

TEST_F(LogicTest, AndOneFalse)
{
    ExprHandle expr = parseAndEvaluate("(and #f)");
    expectBoolean(expr, false, "#f");
}

TEST_F(LogicTest, AndManyTrue)
{
    ExprHandle expr = parseAndEvaluate("(and #t #t 3)");
    expectInteger(expr, 3, "3");
}

TEST_F(LogicTest, AndShortCircuits)
{
    ExprHandle expr = parseAndEvaluate("(and #f (/ 1 0))");
    expectBoolean(expr, false, "#f");
}

TEST_F(LogicTest, AndComplex1)
{
    ExprHandle expr = parseAndEvaluate("(and 2 (and 3 4))");
    expectInteger(expr, 4, "4");
}

TEST_F(LogicTest, AndComplex2)
{
    ExprHandle expr = parseAndEvaluate("(and (and 3 4) 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(LogicTest, OrZeroForms)
{
    ExprHandle expr = parseAndEvaluate("(or)");
    expectBoolean(expr, false, "#f");
}

TEST_F(LogicTest, OrOneTrue)
{
    ExprHandle expr = parseAndEvaluate("(or 3)");
    expectInteger(expr, 3, "3");
}

TEST_F(LogicTest, OrOneFalse)
{
    ExprHandle expr = parseAndEvaluate("(or #f)");
    expectBoolean(expr, false, "#f");
}

TEST_F(LogicTest, OrManyTrue)
{
    ExprHandle expr = parseAndEvaluate("(or #t #t 3)");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, OrShortCircuits)
{
    ExprHandle expr = parseAndEvaluate("(or #t (/ 1 0))");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, OrComplex1)
{
    ExprHandle expr = parseAndEvaluate("(or #f (or 3 4))");
    expectInteger(expr, 3, "3");
}

TEST_F(LogicTest, OrComplex2)
{
    ExprHandle expr = parseAndEvaluate("(or (or #f #f) 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(LogicTest, NotZeroForms)
{
    ExprHandle expr = parseAndEvaluate("(not)");
    expectError(expr);
}

TEST_F(LogicTest, NotOneTrue)
{
    ExprHandle expr = parseAndEvaluate("(not 3)");
    expectBoolean(expr, false, "#f");
}

TEST_F(LogicTest, NotOneFalse)
{
    ExprHandle expr = parseAndEvaluate("(not #f)");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, NotManyTrue)
{
    ExprHandle expr = parseAndEvaluate("(not #t #t 3)");
    expectError(expr);
}

