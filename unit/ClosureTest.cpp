
#include "TestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class ClosureTest : public TestBase
{
};

TEST_F(ClosureTest, ClosureBasic)
{
    ScamExpr * parm = ExpressionFactory::makeNil();
    ScamExpr * two  = ExpressionFactory::makeInteger(2);
    ScamExpr * forms = ExpressionFactory::makeList(two);
    ScamExpr * expr =
      ExpressionFactory::makeClosure(parm, forms, engine.getFrame());

    expectProcedure(expr, "(lambda () (2))");

    ScamExpr * args = ExpressionFactory::makeNil();
    ScamExpr * finalValue = apply(expr, args);

    expectInteger(finalValue, 2, "2");
}

TEST_F(ClosureTest, ClosureMultipleForms)
{
    ScamExpr * parm = ExpressionFactory::makeNil();

    ScamExpr * two  = ExpressionFactory::makeInteger(2);
    ScamExpr * zed  = ExpressionFactory::makeCharacter("\\#z");
    ScamExpr * forms = ExpressionFactory::makeList(two, zed);
    ScamExpr * expr =
      ExpressionFactory::makeClosure(parm, forms, engine.getFrame());

    expectProcedure(expr, "(lambda () (2 \\#z))");

    ScamExpr * args = ExpressionFactory::makeNil();
    ScamExpr * finalValue = apply(expr, args);

    expectChar(finalValue, 'z', "\\#z");
}

TEST_F(ClosureTest, ClosureWithArg)
{
    ScamExpr * argx = ExpressionFactory::makeSymbol("x");
    ScamExpr * parm = ExpressionFactory::makeList(argx);

    ScamExpr * plus = ExpressionFactory::makeSymbol("+");

    ScamExpr * form1 =
      ExpressionFactory::makeList(plus, argx, argx);
    ScamExpr * forms = ExpressionFactory::makeList(form1);

    ScamExpr * expr =
      ExpressionFactory::makeClosure(parm, forms, engine.getFrame());

    expectProcedure(expr, "(lambda (x) ((+ x x)))");

    ScamExpr * arg3 = ExpressionFactory::makeInteger(3);
    ScamExpr * args = ExpressionFactory::makeList(arg3);
    try {
        ScamExpr * finalValue = apply(expr, args);
        expectInteger(finalValue, 6, "6");
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage();
    }
}

TEST_F(ClosureTest, LambdaBasic)
{
    ScamExpr * expr = parseAndEvaluate("(lambda () 2)");
    expectProcedure(expr, "(lambda () (2))");
}

TEST_F(ClosureTest, LambdaEvalConst)
{
    ScamExpr * expr = parseAndEvaluate("((lambda () 2))");
    expectInteger(expr, 2, "2");
}

TEST_F(ClosureTest, LambdaEvalWithArg)
{
    ScamExpr * expr = parseAndEvaluate("((lambda (x) (* x 2)) (+ 1 3))");
    expectInteger(expr, 8, "8");
}

TEST_F(ClosureTest, LambdaCaptures)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/capture.scm");
    expectInteger(expr, 20, "20");
}

TEST_F(ClosureTest, LambdaFormalsMaskEnv)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/formalsmask.scm");
    expectFloat(expr, 0.5, "0.5");

    expr = parseAndEvaluate("x");
    expectFloat(expr, 0.0, "0");
}

TEST_F(ClosureTest, LambdaTooFewActuals)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/toofew.scm");
    expectError(expr);
}

TEST_F(ClosureTest, LambdaTooManyActuals)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/toomany.scm");
    expectError(expr);
}

TEST_F(ClosureTest, LambdaDottedParmListZero)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/dottedzero.scm");
    expectNil(expr);
}

TEST_F(ClosureTest, LambdaDottedParmListOne)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/dottedone.scm");
    expectList(expr, "(2)", 1);
}

TEST_F(ClosureTest, LambdaDottedParmListSeveral)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/dottedmany.scm");
    expectList(expr, "(2 4 #t)", 3);
}

TEST_F(ClosureTest, LambdaSymbolParmListNone)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/listnone.scm");
    expectNil(expr);
}

TEST_F(ClosureTest, LambdaSymbolParmListOne)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/listone.scm");
    expectList(expr, "(5)", 1);
}

TEST_F(ClosureTest, LambdaSymbolParmListSeveral)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/closure/listmany.scm");
    expectList(expr, "(5 10 15)", 3);
}

TEST_F(ClosureTest, MacroBasic)
{
    ScamExpr * expr = parseAndEvaluate("(macro () 2)");
    expectProcedure(expr, "(macro () (2))");
}
