
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
    expectString(expr, "Success");
}
