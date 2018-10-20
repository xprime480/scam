
#include "ExpressionTestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class LetTest : public ExpressionTestBase
{
};

TEST_F(LetTest, LetSimple)
{
    ExprHandle expr = parseAndEvaluate("(let ((x 1)) (* x 2))");
    expectInteger(expr, 2, "2");
}

TEST_F(LetTest, LetNoBindings)
{
    ExprHandle expr = parseAndEvaluate("(let () 3)");
    expectInteger(expr, 3, "3");
}

TEST_F(LetTest, LetSeveralBindings)
{
    ExprHandle expr = parseAndEvaluate("(let ((a 3) (b 5)) (* a b))");
    expectInteger(expr, 15, "15");
}

TEST_F(LetTest, LetSeveralForms)
{
    ExprHandle expr = parseAndEvaluate("(let () 3 5 9)");
    expectInteger(expr, 9, "9");
}

TEST_F(LetTest, LetCreatesNewEnv)
{
    parseAndEvaluate("(define x 2)");
    parseAndEvaluate("(define y 0)");
    ExprHandle expr = parseAndEvaluate("(let ((y 1.0)) (/ x y))");
    expectFloat(expr, 2.0, "2");

    expr = parseAndEvaluate("y");
    expectInteger(expr, 0, "0");
}

TEST_F(LetTest, LetParallelEvaluation)
{
    parseAndEvaluate("(define x 2)");
    parseAndEvaluate("(define y 0)");
    ExprHandle expr = parseAndEvaluate("(let ((x 1) (y x)) y)");
    expectInteger(expr, 2, "2");
}

TEST_F(LetTest, LetNoForms)
{
    ExprHandle expr = parseAndEvaluate("(let ())");
    expectNil(expr);
}

TEST_F(LetTest, LetBadBindings1)
{
    try {
    ExprHandle expr = parseAndEvaluate("(let x 2)");
    expectError(expr);
    }
    catch ( ScamException e ) {
	FAIL() << e.getMessage();
    }
}

TEST_F(LetTest, LetBadBindings2)
{
    ExprHandle expr = parseAndEvaluate("(let (3 2) \"foo\")");
    expectError(expr);
}

TEST_F(LetTest, LetBadBindings3)
{
    ExprHandle expr = parseAndEvaluate("(let ((3 2)) \"foo\")");
    expectError(expr);
}

TEST_F(LetTest, LetBadBindings4)
{
    ExprHandle expr = parseAndEvaluate("(let ((x)) \"foo\")");
    expectError(expr);
}

TEST_F(LetTest, LetStarIsSequential)
{
    parseAndEvaluate("(define x 2)");
    parseAndEvaluate("(define y 0)");
    ExprHandle expr = parseAndEvaluate("(let* ((x 1) (y x)) y)");
    expectInteger(expr, 1, "1");
}
