
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
    ExprHandle evalExpression(string const & input)
    {
        StringTokenizer tokenizer(input);
        ScamParser parser(tokenizer);

        parser.parseExpr(extractor);
        ExprHandle expr = extractor->getExpr();

        env = ScamEngine::getStandardEnv();

        ExprHandle rv;
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

TEST_F(MathTest, AddZeroArgs)
{
    ExprHandle expr = evalExpression("(+)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, AddOneArg)
{
    ExprHandle expr = evalExpression("(+ 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(MathTest, AddTwoArgs)
{
    ExprHandle expr = evalExpression("(+ 2 2)");
    expectInteger(expr, 4, "4");
}

TEST_F(MathTest, AddManyArgs)
{
    ExprHandle expr = evalExpression("(+ 2 2 -1 -3 4)");
    expectInteger(expr, 4, "4");
}

TEST_F(MathTest, AddTypeUnification)
{
    ExprHandle expr = evalExpression("(+ 2 2.0)");
    expectFloat(expr, 4, "4");
}

TEST_F(MathTest, AddBadArgument)
{
    ExprHandle expr = evalExpression("(+ 2 #f)");
    expectError(expr);
}

TEST_F(MathTest, SubZeroArgs)
{
    ExprHandle expr = evalExpression("(-)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, SubOneArg)
{
    ExprHandle expr = evalExpression("(- 2)");
    expectInteger(expr, -2, "-2");
}

TEST_F(MathTest, SubTwoArgs)
{
    ExprHandle expr = evalExpression("(- 2 2)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, SubManyArgs)
{
    ExprHandle expr = evalExpression("(- 2 2 -1 -3 4)");
    expectInteger(expr, 0, "0");
}

TEST_F(MathTest, SubTypeUnification)
{
    ExprHandle expr = evalExpression("(- 2 1.0)");
    expectFloat(expr, 1, "1");
}

TEST_F(MathTest, SubBadArgument)
{
    ExprHandle expr = evalExpression("(- 2 #f)");
    expectError(expr);
}

