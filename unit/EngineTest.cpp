
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class EngineTest : public ExpressionTestBase
{
};

TEST_F(EngineTest, BacktrackInitial)
{
    ExprHandle expr = parseAndEvaluate("?");
    expectError(expr, "No current backtrack context");
}

TEST_F(EngineTest, AmbZeroForms)
{
    ExprHandle expr = parseAndEvaluate("(amb)");
    expectError(expr, "No more choices");
}

TEST_F(EngineTest, AmbOneFormFirst)
{
    ExprHandle expr = parseAndEvaluate("(amb 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(EngineTest, AmbOneFormSecond)
{
    ExprHandle expr = parseAndEvaluate("(amb 2)");
    expr = parseAndEvaluate("?");
    expectError(expr, "No more choices");
}

TEST_F(EngineTest, AmbOneFormThird)
{
    parseAndEvaluate("(amb 2)");
    parseAndEvaluate("?");
    ExprHandle expr = parseAndEvaluate("?");
    expectError(expr, "No current backtrack context");
}

TEST_F(EngineTest, AmbManyFormSecond)
{
    ExprHandle expr = parseAndEvaluate("(amb 2 3 4)");
    expr = parseAndEvaluate("?");
    expectInteger(expr, 3, "3");
}

TEST_F(EngineTest, AmbNestedLet)
{
    ExprHandle expr = parseAndEvaluate("(load \"scripts/system/nestedamb.scm\")");
    expectList(expr, "(a 1)", 2);

    expr = parseAndEvaluate("?");
    expectList(expr, "(a 2)", 2);

    expr = parseAndEvaluate("?");
    expectList(expr, "(b 1)", 2);

    expr = parseAndEvaluate("?");
    expectList(expr, "(b 2)", 2);

    expr = parseAndEvaluate("?");
    expectError(expr, "No more choices");

    expr = parseAndEvaluate("?");
    expectError(expr, "No current backtrack context");

    expr = parseAndEvaluate("?");
    expectError(expr, "No current backtrack context");
}

TEST_F(EngineTest, AmbNestedLetStar)
{
    ExprHandle expr = parseAndEvaluate("(load \"scripts/system/nestedamb2.scm\")");
    expectList(expr, "(a 1)", 2);

    expr = parseAndEvaluate("?");
    expectList(expr, "(a 2)", 2);

    expr = parseAndEvaluate("?");
    expectList(expr, "(b 1)", 2);

    expr = parseAndEvaluate("?");
    expectList(expr, "(b 2)", 2);

    expr = parseAndEvaluate("?");
    expectError(expr, "No more choices");

    expr = parseAndEvaluate("?");
    expectError(expr, "No current backtrack context");

    expr = parseAndEvaluate("?");
    expectError(expr, "No current backtrack context");
}

TEST_F(EngineTest, AmbPrimitive)
{
    ExprHandle expr = parseAndEvaluate("(+ 2 (amb 1 2 3))");
    expectInteger(expr, 3, "3");

    expr = parseAndEvaluate("?");
    expectInteger(expr, 4, "4");

    expr = parseAndEvaluate("?");
    expectInteger(expr, 5, "5");

    expr = parseAndEvaluate("?");
    expectError(expr, "No more choices");

    expr = parseAndEvaluate("?");
    expectError(expr, "No current backtrack context");
}

TEST_F(EngineTest, AmbDefine)
{
    ExprHandle expr = parseAndEvaluate("(define test (amb 3 4)) test ? test");
    expectInteger(expr, 4, "4");

    expr = parseAndEvaluate("? test");
    expectError(expr, "Symbol test does not exist in the current environment");
}

TEST_F(EngineTest, AmbAssign)
{
    ExprHandle expr = parseAndEvaluate("(load \"scripts/system/ambassign.scm\")");
    expectInteger(expr, 1, "1");
}

TEST_F(EngineTest, AmbUndefine)
{
    // no use case for this for now
    //
    return;

    ExprHandle expr = parseAndEvaluate("(load \"scripts/system/ambundef.scm\")");
    expectInteger(expr, 7, "7");
}

TEST_F(EngineTest, AmbOperator)
{
    ExprHandle expr = parseAndEvaluate("((amb + -) 2 2)");
    expectInteger(expr, 4, "4");

    expr = parseAndEvaluate("?");
    expectInteger(expr, 0, "0");
}

TEST_F(EngineTest, RequireTest)
{
    ExprHandle expr = parseAndEvaluate("(load \"scripts/system/require.scm\")");
    expectInteger(expr, 2, "2");

    expr = parseAndEvaluate("?");
    expectInteger(expr, 4, "4");

    expr = parseAndEvaluate("?");
    expectInteger(expr, 6, "6");
}