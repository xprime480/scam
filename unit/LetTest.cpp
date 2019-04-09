
#include "TestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class LetTest : public TestBase
              , public ::testing::WithParamInterface<const char *>
{
protected:
    ScamExpr * runTest(char const * fmt)
    {
        char buf[256];
        sprintf(buf, fmt, GetParam());
        return parseAndEvaluate(buf);
    }
};

TEST_P(LetTest, LetSimple)
{
    ScamExpr * expr = runTest("(%s ((x 1)) (* x 2))");
    expectInteger(expr, 2, "2");
}

TEST_P(LetTest, LetNoBindings)
{
    ScamExpr * expr = runTest("(%s () 3)");
    expectInteger(expr, 3, "3");
}

TEST_P(LetTest, LetSeveralBindings)
{
    ScamExpr * expr = runTest("(%s ((a 3) (b 5)) (* a b))");
    expectInteger(expr, 15, "15");
}

TEST_P(LetTest, LetSeveralForms)
{
    ScamExpr * expr = runTest("(%s () 3 5 9)");
    expectInteger(expr, 9, "9");
}

TEST_P(LetTest, LetCreatesNewEnv)
{
    parseAndEvaluate("(define x 2)");
    parseAndEvaluate("(define y 0)");
    ScamExpr * expr = runTest("(%s ((y 1.0)) (/ x y))");
    expectFloat(expr, 2.0, "2");

    expr = parseAndEvaluate("y");
    expectInteger(expr, 0, "0");
}

TEST_P(LetTest, LetNoForms)
{
    ScamExpr * expr = runTest("(%s ())");
    expectNil(expr);
}

TEST_P(LetTest, LetBadBindings1)
{
    ScamExpr * expr = runTest("(%s x 2)");
    expectError(expr);
}

TEST_P(LetTest, LetBadBindings2)
{
    ScamExpr * expr = runTest("(%s (3 2) \"foo\")");
    expectError(expr);
}

TEST_P(LetTest, LetBadBindings3)
{
    ScamExpr * expr = runTest("(%s ((3 2)) \"foo\")");
    expectError(expr);
}

TEST_P(LetTest, LetBadBindings4)
{
    ScamExpr * expr = runTest("(%s ((x)) \"foo\")");
    expectError(expr);
}

TEST_P(LetTest, LetDependentForms)
{
    parseAndEvaluate("(define x 2)");
    parseAndEvaluate("(define y 0)");
    ScamExpr * expr = runTest("(%s ((x 1) (y x)) y)");

    if ( 0 == strcmp("let*", GetParam()) ) {
        expectInteger(expr, 1, "1");
    }
    else {
        expectInteger(expr, 2, "2");
    }
}

TEST_P(LetTest, LetRecursiveLambda)
{
    ScamExpr * expr = runTest("\
(%s  ((factorial\
      (lambda (n)\
              (if (> n 1)\
                  (* n (factorial (- n 1)))\
                  1))))\
     (factorial 3))\
");

    if ( 0 == strcmp("letrec", GetParam()) ) {
        expectInteger(expr, 6, "6");
    }
    else {
        expectError(expr);
    }
}

auto values = ::testing::Values("let", "let*", "letrec");
INSTANTIATE_TEST_CASE_P(Let, LetTest, values);
