
#include "ExpressionTestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class ListTest : public ExpressionTestBase
{
};

TEST_F(ListTest, ListNil)
{
    ExprHandle expr = parseAndEvaluate("()");
    expectNil(expr);
}

TEST_F(ListTest, ListEmpty)
{
    ExprHandle expr = parseAndEvaluate("(list)");
    expectNil(expr);
}

TEST_F(ListTest, ListNonEmpty)
{
    ExprHandle expr = parseAndEvaluate("(list 1 2 3)");
    expectList(expr, "(1 2 3)", 3);
}

TEST_F(ListTest, ConsBasic)
{
    ExprHandle expr = parseAndEvaluate("(cons 2 3)");
    expectCons(expr, "(2 . 3)");
}

TEST_F(ListTest, ConsNoArgs)
{
    ExprHandle expr = parseAndEvaluate("(cons)");
    expectError(expr);
}

TEST_F(ListTest, ConsOneArg)
{
    ExprHandle expr = parseAndEvaluate("(cons 3)");
    expectError(expr);
}

TEST_F(ListTest, ConsThreeArgs)
{
    ExprHandle expr = parseAndEvaluate("(cons 3 5 7)");
    expectError(expr);
}

TEST_F(ListTest, CdrBasic)
{
    ExprHandle expr = parseAndEvaluate("(cdr (list 3 5 7))");
    expectList(expr, "(5 7)", 2);
}

TEST_F(ListTest, CdrSingleton)
{
    ExprHandle expr = parseAndEvaluate("(cdr (list 7))");
    expectNil(expr);
}

TEST_F(ListTest, CdrOfDottedPair)
{
    expectFalse("(cdr '(#t . #f))");
}

TEST_F(ListTest, CdrEmptyList)
{
    ExprHandle expr = parseAndEvaluate("(cdr '())");
    expectError(expr);
}

TEST_F(ListTest, CdrNoArgs)
{
    ExprHandle expr = parseAndEvaluate("(cdr)");
    expectError(expr);
}

TEST_F(ListTest, CdrExtraArgs)
{
    ExprHandle expr = parseAndEvaluate("(cdr '(a b c) 2)");
    expectError(expr);
}

TEST_F(ListTest, CdrNonCons)
{
    ExprHandle expr = parseAndEvaluate("(cdr 2)");
    expectError(expr);
}

