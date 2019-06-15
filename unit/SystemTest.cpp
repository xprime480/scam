#include "TestBase.hpp"

using namespace std;
using namespace scam;

class SystemTest : public TestBase
{
};

TEST_F(SystemTest, LoadTest)
{
    ScamValue expr = readEval("(load \"scripts/system/loadtest.scm\")");
    expectInteger(expr, 66, "66", true);
}

TEST_F(SystemTest, IncludeTest)
{
    ScamValue expr = readEval("(include \"scripts/system/includetest.scm\")");
    expectInteger(expr, 66, "66", true);
}

TEST_F(SystemTest, WhileTest)
{
    ScamValue expr = readEval("(load \"scripts/system/whiletest.scm\")");
    expectInteger(expr, 15, "15", true);
}

TEST_F(SystemTest, MapTest)
{
    ScamValue expr = readEval("(load \"scripts/system/maptest.scm\")");
    expectList(expr, "(2 4 6)", 3);
}

TEST_F(SystemTest, SpawnTest)
{
    ScamValue expr = readEval("(load \"scripts/system/spawntest.scm\")");
    expectList(expr, "(1 2)", 2);
}
