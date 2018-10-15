
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class ClosureTest : public ExpressionTestBase
{
};

TEST_F(ClosureTest, ClosureBasic)
{
    ExprHandle parm = ExpressionFactory::makeNil();
    ExprHandle nil  = ExpressionFactory::makeNil();
    ExprHandle two  = ExpressionFactory::makeInteger(2);
    ExprHandle forms = ExpressionFactory::makeCons(two, nil);
    ExprHandle expr = ExpressionFactory::makeClosure(parm, forms, env);

    expectProcedure(expr, "(proc () (2))");

    ExprHandle args = ExpressionFactory::makeNil();
    expr->apply(args, extractor, env);
    ExprHandle final = extractor->getExpr();

    expectInteger(final, 2, "2");
}

TEST_F(ClosureTest, LambdaBasic)
{
    ExprHandle expr = parseAndEvaluate("(lambda () 2)");
    expectProcedure(expr, "(proc () (2))");
}
