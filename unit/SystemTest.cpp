
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
