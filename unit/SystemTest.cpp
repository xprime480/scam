
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class SystemTest : public ExpressionTestBase
{
};

TEST_F(SystemTest, LoadTest)
{
    ScamExpr * expr
        = parseAndEvaluate("(load \"scripts/system/loadtest.scm\")");
    expectInteger(expr, 66, "66");
}

TEST_F(SystemTest, WhileTest)
{
    ScamExpr * expr
        = parseAndEvaluate("(load \"scripts/system/whiletest.scm\")");
    expectInteger(expr, 15, "15");
}

TEST_F(SystemTest, MapTest)
{
    ScamExpr * expr
        = parseAndEvaluate("(load \"scripts/system/maptest.scm\")");
    expectList(expr, "(2 4 6)", 3);
}

TEST_F(SystemTest, SpawnTest)
{
    ScamExpr * expr
        = parseAndEvaluate("(load \"scripts/system/spawntest.scm\")");
    expectList(expr, "(1 2)", 2);
}

TEST_F(SystemTest, ErrorTestZero)
{
    ScamExpr * expr = parseAndEvaluate("(error)");
    expectError(expr, "Error detected");
}

TEST_F(SystemTest, ErrorTestOne)
{
    ScamExpr * expr = parseAndEvaluate("(error \"bagels are not donughts\")");
    expectError(expr, "bagels are not donughts");
}

TEST_F(SystemTest, ErrorTestThree)
{
    ScamExpr * expr = parseAndEvaluate("(error 1 2 3)");
    expectError(expr, "[1] 1\n[2] 2\n[3] 3\n");
}
