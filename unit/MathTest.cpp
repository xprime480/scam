
#include "ExpressionTestBase.hpp"

#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

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

	shared_ptr<ScamExpr> rv;
	try {
	    rv = evaluate(expr);
	}
	catch ( ScamException e ) {
	    stringstream s;
	    s << "Unhandled exception: " << e.getMessage();
	    rv = ExpressionFactory::makeError(s.str());
	}
	return rv;
    }
};

TEST_F(MathTest, PlusZeroArgs)
{
    shared_ptr<ScamExpr> expr = evalExpression("(+)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, PlusOneArg)
{
    shared_ptr<ScamExpr> expr = evalExpression("(+ 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(MathTest, PlusTwoArgs)
{
    shared_ptr<ScamExpr> expr = evalExpression("(+ 2 2)");
    expectInteger(expr, 4, "4");
}

TEST_F(MathTest, PlusManyArgs)
{
    shared_ptr<ScamExpr> expr = evalExpression("(+ 2 2 -1 -3 4)");
    expectInteger(expr, 4, "4");
}

TEST_F(MathTest, PlusTypeUnification)
{
    shared_ptr<ScamExpr> expr = evalExpression("(+ 2 2.0)");
    expectFloat(expr, 4, "4");
}

TEST_F(MathTest, PlusBadArgument)
{
    shared_ptr<ScamExpr> expr = evalExpression("(+ 2 #f)");
    expectError(expr);
}

