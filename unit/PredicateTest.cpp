
#include "ExpressionTestBase.hpp"

#include <sstream>

using namespace std;
using namespace scam;

class PredicateTest : public ExpressionTestBase
{
};

TEST_F(PredicateTest, NilTestNil)
{
    ExprHandle expr = parseAndEvaluate("(nil? ())");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, NilTestNotNil)
{
    ExprHandle expr = parseAndEvaluate("(nil? 2)");
    booleanTest(expr, false, "#f");
}
