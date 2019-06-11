#include "TestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class ListTest : public TestBase
{
};

TEST_F(ListTest, ListNil)
{
    ScamValue expr = parseAndEvaluate("()");
    expectNil(expr);
}

TEST_F(ListTest, ListEmpty)
{
    ScamValue expr = parseAndEvaluate("(list)");
    expectNil(expr);
}

TEST_F(ListTest, ListNonEmpty)
{
    ScamValue expr = parseAndEvaluate("(list 1 2 3)");
    expectList(expr, "(1 2 3)", 3);
}

TEST_F(ListTest, ConsBasic)
{
    ScamValue expr = parseAndEvaluate("(cons 2 3)");
    expectPair(expr, "(2 . 3)");
}

TEST_F(ListTest, ConsNoArgs)
{
    ScamValue expr = parseAndEvaluate("(cons)");
    expectError(expr);
}

TEST_F(ListTest, ConsOneArg)
{
    ScamValue expr = parseAndEvaluate("(cons 3)");
    expectError(expr);
}

TEST_F(ListTest, ConsThreeArgs)
{
    ScamValue expr = parseAndEvaluate("(cons 3 5 7)");
    expectError(expr);
}

TEST_F(ListTest, CdrBasic)
{
    ScamValue expr = parseAndEvaluate("(cdr (list 3 5 7))");
    expectList(expr, "(5 7)", 2);
}

TEST_F(ListTest, CdrSingleton)
{
    ScamValue expr = parseAndEvaluate("(cdr (list 7))");
    expectNil(expr);
}

TEST_F(ListTest, CdrOfDottedPair)
{
    expectFalse("(cdr '(#t . #f))");
}

TEST_F(ListTest, CdrEmptyList)
{
    ScamValue expr = parseAndEvaluate("(cdr '())");
    expectError(expr);
}

TEST_F(ListTest, CdrNoArgs)
{
    ScamValue expr = parseAndEvaluate("(cdr)");
    expectError(expr);
}

TEST_F(ListTest, CdrExtraArgs)
{
    ScamValue expr = parseAndEvaluate("(cdr '(a b c) 2)");
    expectError(expr);
}

TEST_F(ListTest, CdrNonCons)
{
    ScamValue expr = parseAndEvaluate("(cdr 2)");
    expectError(expr);
}
