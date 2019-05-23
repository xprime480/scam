#include "TestBase.hpp"

#include "ScamException.hpp"
#include "input/LambdaParser.hpp"

using namespace std;
using namespace scam;

class ClosureTest : public TestBase
{
};

TEST_F(ClosureTest, ClosureBasic)
{
    LambdaParser * lambda = mm.make<LambdaParser>();
    ScamValue form = readString("(() 2)");
    bool accept = lambda->accept(form);
    ASSERT_TRUE(accept);

    ScamValue expr = ExpressionFactory::makeClosure(lambda, engine.getFrame());
    expectProcedure(expr, "(lambda () 2)");

    ScamValue args = ExpressionFactory::makeNil();
    ScamValue finalValue = apply(expr, args);

    expectInteger(finalValue, 2, "2", true);
}

TEST_F(ClosureTest, ClosureMultipleForms)
{
    LambdaParser * lambda = mm.make<LambdaParser>();
    ScamValue form = readString("(() 2 #\\z)");
    bool accept = lambda->accept(form);
    ASSERT_TRUE(accept);

    ScamValue expr = ExpressionFactory::makeClosure(lambda, engine.getFrame());
    expectProcedure(expr, "(lambda () 2 #\\z)");

    ScamValue args = ExpressionFactory::makeNil();
    ScamValue finalValue = apply(expr, args);

    expectChar(finalValue, 'z', "#\\z");
}

TEST_F(ClosureTest, ClosureWithArg)
{
    LambdaParser * lambda = mm.make<LambdaParser>();
    ScamValue form = readString("((x) (+ x x))");
    bool accept = lambda->accept(form);
    ASSERT_TRUE(accept);

    ScamValue expr = ExpressionFactory::makeClosure(lambda, engine.getFrame());
    expectProcedure(expr, "(lambda (x) (+ x x))");

    ScamValue args = readString("(3)");
    ScamValue finalValue = apply(expr, args);
    expectInteger(finalValue, 6, "6", true);
}

TEST_F(ClosureTest, LambdaBasic)
{
    ScamValue expr = parseAndEvaluate("(lambda () 2)");
    expectProcedure(expr, "(lambda () 2)");
}

TEST_F(ClosureTest, LambdaEvalConst)
{
    ScamValue expr = parseAndEvaluate("((lambda () 2))");
    expectInteger(expr, 2, "2", true);
}

TEST_F(ClosureTest, LambdaEvalWithArg)
{
    ScamValue expr = parseAndEvaluate("((lambda (x) (* x 2)) (+ 1 3))");
    expectInteger(expr, 8, "8", true);
}

TEST_F(ClosureTest, LambdaCaptures)
{
    ScamValue expr = parseAndEvaluateFile("scripts/closure/capture.scm");
    expectInteger(expr, 20, "20", true);
}

TEST_F(ClosureTest, LambdaFormalsMaskEnv)
{
    ScamValue expr = parseAndEvaluateFile("scripts/closure/formalsmask.scm");
    expectRational(expr, pair<int,int>(1,2), "1/2", false);

    expr = parseAndEvaluate("x");
    expectInteger(expr, 0, "0", false);
}

TEST_F(ClosureTest, LambdaTooFewActuals)
{
    ScamValue expr = parseAndEvaluateFile("scripts/closure/toofew.scm");
    expectError(expr);
}

TEST_F(ClosureTest, LambdaTooManyActuals)
{
    ScamValue expr = parseAndEvaluateFile("scripts/closure/toomany.scm");
    expectError(expr);
}

TEST_F(ClosureTest, LambdaDottedParmListZero)
{
    ScamValue expr =
        parseAndEvaluateFile("scripts/closure/dottedzero.scm");
    expectNil(expr);
}

TEST_F(ClosureTest, LambdaDottedParmListOne)
{
    ScamValue expr = parseAndEvaluateFile("scripts/closure/dottedone.scm");
    expectList(expr, "(2)", 1);
}

TEST_F(ClosureTest, LambdaDottedParmListSeveral)
{
    ScamValue expr =
        parseAndEvaluateFile("scripts/closure/dottedmany.scm");
    expectList(expr, "(2 4 #t)", 3);
}

TEST_F(ClosureTest, LambdaSymbolParmListNone)
{
    ScamValue expr = parseAndEvaluateFile("scripts/closure/listnone.scm");
    expectNil(expr);
}

TEST_F(ClosureTest, LambdaSymbolParmListOne)
{
    ScamValue expr = parseAndEvaluateFile("scripts/closure/listone.scm");
    expectList(expr, "(5)", 1);
}

TEST_F(ClosureTest, LambdaSymbolParmListSeveral)
{
    ScamValue expr = parseAndEvaluateFile("scripts/closure/listmany.scm");
    expectList(expr, "(5 10 15)", 3);
}

TEST_F(ClosureTest, LambdaNoFormalsOrBody)
{
    ScamValue expr = parseAndEvaluate("(lambda)");
    expectError(expr, "Expected (lambda args body*); got: (lambda)");
}

TEST_F(ClosureTest, LambdaFormalsNotListorSymbol)
{
    ScamValue expr = parseAndEvaluate("(lambda 2 (+ 2 2))");
    expectError(expr, "Formals should be list or symbol; got: 2");
}

TEST_F(ClosureTest, LambdaParameterNotSymbol)
{
    ScamValue expr = parseAndEvaluate("(lambda (:keyword) (+ 2 2))");
    expectError(expr, "Formal parameter should be a symbol; got: :keyword");
}

TEST_F(ClosureTest, LambdaDuplicateParameter)
{
    ScamValue expr = parseAndEvaluate("(lambda (x x) (* x 2))");
    expectError(expr, "Symbol cannot appear twice in formals list: x");
}

TEST_F(ClosureTest, LambdaDuplicateParameterInImproperList)
{
    ScamValue expr = parseAndEvaluate("(lambda (x . x) (* x 2))");
    expectError(expr, "Symbol cannot appear twice in formals list: x");
}

TEST_F(ClosureTest, LambdaParameterNotSymbolInImproperList)
{
    ScamValue expr = parseAndEvaluate("(lambda (ok . :keyword) (+ 2 2))");
    expectError(expr, "Formal parameter should be a symbol; got: :keyword");
}

TEST_F(ClosureTest, MacroBasic)
{
    ScamValue expr = parseAndEvaluate("(macro () 2)");
    expectProcedure(expr, "(macro () 2)");
}

TEST_F(ClosureTest, MacroNoFormalsOrBody)
{
    ScamValue expr = parseAndEvaluate("(macro)");
    expectError(expr, "Expected (macro args body*); got: (macro)");
}
