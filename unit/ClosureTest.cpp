#include "TestBase.hpp"

#include "ScamException.hpp"
#include "input/LambdaParser.hpp"

#include "util/DebugTrace.hpp"

using namespace std;
using namespace scam;

class ClosureTest : public TestBase
{
};

TEST_F(ClosureTest, ClosureBasic)
{
    LambdaParser * lambda = mm.make<LambdaParser>();
    ExprHandle form = readString("(() 2)");
    bool accept = lambda->accept(form);
    ASSERT_TRUE(accept);

    ExprHandle expr = ExpressionFactory::makeClosure(lambda, engine.getFrame());
    expectProcedure(expr, "(lambda () 2)");

    ExprHandle args = ExpressionFactory::makeNil();
    ExprHandle finalValue = apply(expr, args);

    expectInteger(finalValue, 2, "2");
}

TEST_F(ClosureTest, ClosureMultipleForms)
{
    LambdaParser * lambda = mm.make<LambdaParser>();
    ExprHandle form = readString("(() 2 #\\z)");
    bool accept = lambda->accept(form);
    ASSERT_TRUE(accept);

    ExprHandle expr = ExpressionFactory::makeClosure(lambda, engine.getFrame());
    expectProcedure(expr, "(lambda () 2 #\\z)");

    ExprHandle args = ExpressionFactory::makeNil();
    ExprHandle finalValue = apply(expr, args);

    expectChar(finalValue, 'z', "#\\z");
}

TEST_F(ClosureTest, ClosureWithArg)
{
    LambdaParser * lambda = mm.make<LambdaParser>();
    ExprHandle form = readString("((x) (+ x x))");
    bool accept = lambda->accept(form);
    ASSERT_TRUE(accept);

    ExprHandle expr = ExpressionFactory::makeClosure(lambda, engine.getFrame());
    expectProcedure(expr, "(lambda (x) (+ x x))");

    ExprHandle args = readString("(3)");
    ExprHandle finalValue = apply(expr, args);
    expectInteger(finalValue, 6, "6");
}

TEST_F(ClosureTest, LambdaBasic)
{
    ExprHandle expr = parseAndEvaluate("(lambda () 2)");
    expectProcedure(expr, "(lambda () 2)");
}

TEST_F(ClosureTest, LambdaEvalConst)
{
    ExprHandle expr = parseAndEvaluate("((lambda () 2))");
    expectInteger(expr, 2, "2");
}

TEST_F(ClosureTest, LambdaEvalWithArg)
{
    ExprHandle expr = parseAndEvaluate("((lambda (x) (* x 2)) (+ 1 3))");
    expectInteger(expr, 8, "8");
}

TEST_F(ClosureTest, LambdaCaptures)
{
    ExprHandle expr =
        parseAndEvaluateFile("scripts/closure/capture.scm");
    expectInteger(expr, 20, "20");
}

TEST_F(ClosureTest, LambdaFormalsMaskEnv)
{
    ExprHandle expr =
        parseAndEvaluateFile("scripts/closure/formalsmask.scm");
    expectReal(expr, 0.5, "0.5");

    expr = parseAndEvaluate("x");
    expectInteger(expr, 0, "0");
}

TEST_F(ClosureTest, LambdaTooFewActuals)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/closure/toofew.scm");
    expectError(expr);
}

TEST_F(ClosureTest, LambdaTooManyActuals)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/closure/toomany.scm");
    expectError(expr);
}

TEST_F(ClosureTest, LambdaDottedParmListZero)
{
    ExprHandle expr =
        parseAndEvaluateFile("scripts/closure/dottedzero.scm");
    expectNil(expr);
}

TEST_F(ClosureTest, LambdaDottedParmListOne)
{
    ExprHandle expr =
        parseAndEvaluateFile("scripts/closure/dottedone.scm");
    expectList(expr, "(2)", 1);
}

TEST_F(ClosureTest, LambdaDottedParmListSeveral)
{
    ExprHandle expr =
        parseAndEvaluateFile("scripts/closure/dottedmany.scm");
    expectList(expr, "(2 4 #t)", 3);
}

TEST_F(ClosureTest, LambdaSymbolParmListNone)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/closure/listnone.scm");
    expectNil(expr);
}

TEST_F(ClosureTest, LambdaSymbolParmListOne)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/closure/listone.scm");
    expectList(expr, "(5)", 1);
}

TEST_F(ClosureTest, LambdaSymbolParmListSeveral)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/closure/listmany.scm");
    expectList(expr, "(5 10 15)", 3);
}

TEST_F(ClosureTest, LambdaNoFormalsOrBody)
{
    ExprHandle expr = parseAndEvaluate("(lambda)");
    expectError(expr, "Expected (lambda args body*); got: (lambda)");
}

TEST_F(ClosureTest, LambdaFormalsNotListorSymbol)
{
    ExprHandle expr = parseAndEvaluate("(lambda 2 (+ 2 2))");
    expectError(expr, "Formals should be list or symbol; got: 2");
}

TEST_F(ClosureTest, LambdaParameterNotSymbol)
{
    ExprHandle expr = parseAndEvaluate("(lambda (:keyword) (+ 2 2))");
    expectError(expr, "Formal parameter should be a symbol; got: :keyword");
}

TEST_F(ClosureTest, LambdaDuplicateParameter)
{
    ExprHandle expr = parseAndEvaluate("(lambda (x x) (* x 2))");
    expectError(expr, "Symbol cannot appear twice in formals list: x");
}

TEST_F(ClosureTest, LambdaDuplicateParameterInImproperList)
{
    ExprHandle expr = parseAndEvaluate("(lambda (x . x) (* x 2))");
    expectError(expr, "Symbol cannot appear twice in formals list: x");
}

TEST_F(ClosureTest, LambdaParameterNotSymbolInImproperList)
{
    ExprHandle expr = parseAndEvaluate("(lambda (ok . :keyword) (+ 2 2))");
    expectError(expr, "Formal parameter should be a symbol; got: :keyword");
}

TEST_F(ClosureTest, MacroBasic)
{
    ExprHandle expr = parseAndEvaluate("(macro () 2)");
    expectProcedure(expr, "(macro () 2)");
}

TEST_F(ClosureTest, MacroNoFormalsOrBody)
{
    ExprHandle expr = parseAndEvaluate("(macro)");
    expectError(expr, "Expected (macro args body*); got: (macro)");
}
