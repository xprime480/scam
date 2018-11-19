
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class SystemTest : public ExpressionTestBase
{
};

TEST_F(SystemTest, LoadTest)
{
    ExprHandle expr
        = parseAndEvaluate("(load \"scripts/system/loadtest.scm\")");
    expectInteger(expr, 66, "66");
}

TEST_F(SystemTest, PreludeTest)
{
    ExprHandle expr = parseAndEvaluate("(max 123 -123)");
    expectInteger(expr, 123, "123");
}

TEST_F(SystemTest, WhileTest)
{
   ExprHandle expr
        = parseAndEvaluate("(load \"scripts/system/whiletest.scm\")");
    expectInteger(expr, 15, "15");
}

TEST_F(SystemTest, MapTest)
{
   ExprHandle expr
        = parseAndEvaluate("(load \"scripts/system/maptest.scm\")");
   expectList(expr, "(2 4 6)", 3);
}
