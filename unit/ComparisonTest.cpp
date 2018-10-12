
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class ComparisonTest : public ExpressionTestBase
{
};

TEST_F(ComparisonTest, EqZeroForms)
{
    ExprHandle expr = parseAndEvaluate("(=)");
    expectBoolean(expr, true, "#t");
}
