#include "TestBase.hpp"

using namespace std;
using namespace scam;

class SystemTest : public TestBase
{
};

TEST_F(SystemTest, LoadTest)
{
    ScamValue expr =
        parseAndEvaluate("(load \"scripts/system/loadtest.scm\")");
    expectInteger(expr, 66, "66", true);
}

TEST_F(SystemTest, IncludeTest)
{
    ScamValue expr =
        parseAndEvaluate("(include \"scripts/system/includetest.scm\")");
    expectInteger(expr, 66, "66", true);
}

TEST_F(SystemTest, WhileTest)
{
    ScamValue expr =
        parseAndEvaluate("(load \"scripts/system/whiletest.scm\")");
    expectInteger(expr, 15, "15", true);
}

TEST_F(SystemTest, MapTest)
{
    ScamValue expr = parseAndEvaluate("(load \"scripts/system/maptest.scm\")");
    expectList(expr, "(2 4 6)", 3);
}

TEST_F(SystemTest, SpawnTest)
{
    ScamValue expr =
        parseAndEvaluate("(load \"scripts/system/spawntest.scm\")");
    expectList(expr, "(1 2)", 2);
}

TEST_F(SystemTest, ErrorTestZero)
{
    ScamValue expr = parseAndEvaluate("(error)");
    expectError(expr, "Error detected");
}

TEST_F(SystemTest, ErrorTestOne)
{
    ScamValue expr = parseAndEvaluate("(error \"bagels are not doughnuts\")");
    expectError(expr, "\"bagels are not doughnuts\"");
}

TEST_F(SystemTest, ErrorTestThree)
{
    ScamValue expr = parseAndEvaluate("(error 1 2 3)");
    expectError(expr, "[1] 1\n[2] 2\n[3] 3\n");
}
