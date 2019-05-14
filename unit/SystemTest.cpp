#include "TestBase.hpp"

using namespace std;
using namespace scam;

class SystemTest : public TestBase
{
};

TEST_F(SystemTest, LoadTest)
{
    ExprHandle expr =
        parseAndEvaluate("(load \"scripts/system/loadtest.scm\")");
    expectInteger(expr, 66, "66", true);
}

TEST_F(SystemTest, IncludeTest)
{
    ExprHandle expr =
        parseAndEvaluate("(include \"scripts/system/includetest.scm\")");
    expectInteger(expr, 66, "66", true);
}

TEST_F(SystemTest, WhileTest)
{
    ExprHandle expr =
        parseAndEvaluate("(load \"scripts/system/whiletest.scm\")");
    expectInteger(expr, 15, "15", true);
}

TEST_F(SystemTest, MapTest)
{
    ExprHandle expr = parseAndEvaluate("(load \"scripts/system/maptest.scm\")");
    expectList(expr, "(2 4 6)", 3);
}

TEST_F(SystemTest, SpawnTest)
{
    ExprHandle expr =
        parseAndEvaluate("(load \"scripts/system/spawntest.scm\")");
    expectList(expr, "(1 2)", 2);
}

TEST_F(SystemTest, ErrorTestZero)
{
    ExprHandle expr = parseAndEvaluate("(error)");
    expectError(expr, "Error detected");
}

TEST_F(SystemTest, ErrorTestOne)
{
    ExprHandle expr = parseAndEvaluate("(error \"bagels are not doughnuts\")");
    expectError(expr, "bagels are not doughnuts");
}

TEST_F(SystemTest, ErrorTestThree)
{
    ExprHandle expr = parseAndEvaluate("(error 1 2 3)");
    expectError(expr, "[1] 1\n[2] 2\n[3] 3\n");
}
