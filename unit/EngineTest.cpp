#include "TestBase.hpp"

using namespace std;
using namespace scam;

class EngineTest : public TestBase
{
};

TEST_F(EngineTest, BacktrackInitial)
{
    ScamValue expr = readEval("?");
    expectError(expr, "No current backtrack context", false);
}

TEST_F(EngineTest, AmbZeroForms)
{
    ScamValue expr = readEval("(amb)");
    expectError(expr, "No more choices", false);
}

TEST_F(EngineTest, AmbOneFormFirst)
{
    ScamValue expr = readEval("(amb 2)");
    expectInteger(expr, 2, "2", true);
}

TEST_F(EngineTest, AmbOneFormSecond)
{
    ScamValue expr = readEval("(amb 2)");
    expr = readEval("?");
    expectError(expr, "No more choices", false);
}

TEST_F(EngineTest, AmbOneFormThird)
{
    readEval("(amb 2)");
    readEval("?");
    ScamValue expr = readEval("?");
    expectError(expr, "No current backtrack context", false);
}

TEST_F(EngineTest, AmbManyFormSecond)
{
    ScamValue expr = readEval("(amb 2 3 4)");
    expr = readEval("?");
    expectInteger(expr, 3, "3", true);
}

TEST_F(EngineTest, AmbNestedLet)
{
    ScamValue expr = readEval("(load \"scripts/system/nestedamb.scm\")");
    expectList(expr, "(a 1)", 2);

    expr = readEval("?");
    expectList(expr, "(a 2)", 2);

    expr = readEval("?");
    expectList(expr, "(b 1)", 2);

    expr = readEval("?");
    expectList(expr, "(b 2)", 2);

    expr = readEval("?");
    expectError(expr, "No more choices", false);

    expr = readEval("?");
    expectError(expr, "No current backtrack context", false);

    expr = readEval("?");
    expectError(expr, "No current backtrack context", false);
}

TEST_F(EngineTest, AmbNestedLetStar)
{
    ScamValue expr = readEval("(load \"scripts/system/nestedamb2.scm\")");
    expectList(expr, "(a 1)", 2);

    expr = readEval("?");
    expectList(expr, "(a 2)", 2);

    expr = readEval("?");
    expectList(expr, "(b 1)", 2);

    expr = readEval("?");
    expectList(expr, "(b 2)", 2);

    expr = readEval("?");
    expectError(expr, "No more choices", false);

    expr = readEval("?");
    expectError(expr, "No current backtrack context", false);

    expr = readEval("?");
    expectError(expr, "No current backtrack context", false);
}

TEST_F(EngineTest, AmbPrimitive)
{
    ScamValue expr = readEval("(+ 2 (amb 1 2 3))");
    expectInteger(expr, 3, "3", true);

    expr = readEval("?");
    expectInteger(expr, 4, "4", true);

    expr = readEval("?");
    expectInteger(expr, 5, "5", true);

    expr = readEval("?");
    expectError(expr, "No more choices", false);

    expr = readEval("?");
    expectError(expr, "No current backtrack context", false);
}

TEST_F(EngineTest, AmbDefine)
{
    ScamValue expr = readEval("(define test (amb 3 4)) test ? test");
    expectInteger(expr, 4, "4", true);

    expr = readEval("? test");
    expectError(expr, "Symbol test does not exist in the current environment");
}

TEST_F(EngineTest, AmbAssign)
{
    ScamValue expr = readEval("(load \"scripts/system/ambassign.scm\")");
    expectInteger(expr, 1, "1", true);
}

TEST_F(EngineTest, AmbUndefine)
{
    // no use case for this for now
    //
    return;

    ScamValue expr = readEval("(load \"scripts/system/ambundef.scm\")");
    expectInteger(expr, 7, "7", true);
}

TEST_F(EngineTest, AmbOperator)
{
    ScamValue expr = readEval("((amb + -) 2 2)");
    expectInteger(expr, 4, "4", true);

    expr = readEval("?");
    expectInteger(expr, 0, "0", true);
}

TEST_F(EngineTest, LoadTwiceTest)
{
    ScamValue expr = readEval("(load \"lib/prelude.scm\")");
    expectError(expr,  "file \"lib/prelude.scm\" already loaded");
}
