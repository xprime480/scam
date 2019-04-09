
#include "TestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class ListTest : public TestBase
{
};

TEST_F(ListTest, ListNil)
{
    ScamExpr * expr = parseAndEvaluate("()");
    expectNil(expr);
}

TEST_F(ListTest, ListEmpty)
{
    ScamExpr * expr = parseAndEvaluate("(list)");
    expectNil(expr);
}

TEST_F(ListTest, ListNonEmpty)
{
    ScamExpr * expr = parseAndEvaluate("(list 1 2 3)");
    expectList(expr, "(1 2 3)", 3);
}

TEST_F(ListTest, ConsBasic)
{
    ScamExpr * expr = parseAndEvaluate("(cons 2 3)");
    expectCons(expr, "(2 . 3)");
}

TEST_F(ListTest, ConsNoArgs)
{
    ScamExpr * expr = parseAndEvaluate("(cons)");
    expectError(expr);
}

TEST_F(ListTest, ConsOneArg)
{
    ScamExpr * expr = parseAndEvaluate("(cons 3)");
    expectError(expr);
}

TEST_F(ListTest, ConsThreeArgs)
{
    ScamExpr * expr = parseAndEvaluate("(cons 3 5 7)");
    expectError(expr);
}

TEST_F(ListTest, CdrBasic)
{
    ScamExpr * expr = parseAndEvaluate("(cdr (list 3 5 7))");
    expectList(expr, "(5 7)", 2);
}

TEST_F(ListTest, CdrSingleton)
{
    ScamExpr * expr = parseAndEvaluate("(cdr (list 7))");
    expectNil(expr);
}

TEST_F(ListTest, CdrOfDottedPair)
{
    expectFalse("(cdr '(#t . #f))");
}

TEST_F(ListTest, CdrEmptyList)
{
    ScamExpr * expr = parseAndEvaluate("(cdr '())");
    expectError(expr);
}

TEST_F(ListTest, CdrNoArgs)
{
    ScamExpr * expr = parseAndEvaluate("(cdr)");
    expectError(expr);
}

TEST_F(ListTest, CdrExtraArgs)
{
    ScamExpr * expr = parseAndEvaluate("(cdr '(a b c) 2)");
    expectError(expr);
}

TEST_F(ListTest, CdrNonCons)
{
    ScamExpr * expr = parseAndEvaluate("(cdr 2)");
    expectError(expr);
}

