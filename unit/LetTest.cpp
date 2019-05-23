#include "TestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class LetTest : public TestBase
              , public ::testing::WithParamInterface<const char *>
{
protected:
    ScamValue runTest(char const * fmt)
    {
        char buf[256];
        sprintf(buf, fmt, GetParam());
        return parseAndEvaluate(buf);
    }
};

TEST_P(LetTest, LetSimple)
{
    ScamValue expr = runTest("(%s ((x 1)) (* x 2))");
    expectInteger(expr, 2, "2", true);
}

TEST_P(LetTest, LetNoBindings)
{
    ScamValue expr = runTest("(%s () 3)");
    expectInteger(expr, 3, "3", true);
}

TEST_P(LetTest, LetSeveralBindings)
{
    ScamValue expr = runTest("(%s ((a 3) (b 5)) (* a b))");
    expectInteger(expr, 15, "15", true);
}

TEST_P(LetTest, LetSeveralForms)
{
    ScamValue expr = runTest("(%s () 3 5 9)");
    expectInteger(expr, 9, "9", true);
}

TEST_P(LetTest, LetCreatesNewEnv)
{
    parseAndEvaluate("(define x 2)");
    parseAndEvaluate("(define y 0)");
    ScamValue expr = runTest("(%s ((y 1.0)) (/ x y))");
    expectInteger(expr, 2, "2", false);

    expr = parseAndEvaluate("y");
    expectInteger(expr, 0, "0", true);
}

TEST_P(LetTest, LetNoForms)
{
    ScamValue expr = runTest("(%s ())");
    expectNil(expr);
}

TEST_P(LetTest, LetBadBindings1)
{
    ScamValue expr = runTest("(%s x 2)");
    expectError(expr);
}

TEST_P(LetTest, LetBadBindings2)
{
    ScamValue expr = runTest("(%s (3 2) \"foo\")");
    expectError(expr);
}

TEST_P(LetTest, LetBadBindings3)
{
    ScamValue expr = runTest("(%s ((3 2)) \"foo\")");
    expectError(expr);
}

TEST_P(LetTest, LetBadBindings4)
{
    ScamValue expr = runTest("(%s ((x)) \"foo\")");
    expectError(expr);
}

TEST_P(LetTest, LetDependentForms)
{
    parseAndEvaluate("(define x 2)");
    parseAndEvaluate("(define y 0)");
    ScamValue expr = runTest("(%s ((x 1) (y x)) y)");

    if ( 0 == strcmp("let*", GetParam()) ) {
        expectInteger(expr, 1, "1", true);
    }
    else {
        expectInteger(expr, 2, "2", true);
    }
}

TEST_P(LetTest, LetRecursiveLambda)
{
    ScamValue expr = runTest("\
(%s  ((factorial\
      (lambda (n)\
              (if (> n 1)\
                  (* n (factorial (- n 1)))\
                  1))))\
     (factorial 3))\
");

    if ( 0 == strcmp("letrec", GetParam()) ) {
        expectInteger(expr, 6, "6", true);
    }
    else {
        expectError(expr);
    }
}

auto values = ::testing::Values("let", "let*", "letrec");
INSTANTIATE_TEST_CASE_P(Let, LetTest, values);
