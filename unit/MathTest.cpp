
#include "ExpressionTestBase.hpp"

#include "ScamEngine.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace std;
using namespace scam;

class MathTest : public ExpressionTestBase
{
protected:
    shared_ptr<ScamExpr> evalExpression(string const & input)
    {
        StringTokenizer tokenizer(input);
        ScamParser parser(tokenizer);

        parser.parseExpr(extractor);
        shared_ptr<ScamExpr> expr = extractor->getExpr();

        env = ScamEngine::getStandardEnv();
        return evaluate(expr);
    }
};

TEST_F(MathTest, PlusTwoArgs)
{
    shared_ptr<ScamExpr> expr = evalExpression("(+ 2 2)");
    expectInteger(expr, 4, "4");
}

