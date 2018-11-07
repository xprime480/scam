
#include "ExpressionTestBase.hpp"

#include "ScamException.hpp"

using namespace std;
using namespace scam;

class ClosureTest : public ExpressionTestBase
{
};

TEST_F(ClosureTest, ClosureBasic)
{
    ExprHandle parm = ExpressionFactory::makeNil();
    ExprHandle two  = ExpressionFactory::makeInteger(2);
    ExprHandle forms = ExpressionFactory::makeList(two.get());
    ExprHandle expr = ExpressionFactory::makeClosure(parm.get(),
                                                     forms.get(),
                                                     env);

    expectProcedure(expr, "(lambda () (2))");

    ExprHandle args = ExpressionFactory::makeNil();
    ExprHandle final = apply(expr, args);

    expectInteger(final, 2, "2");
}

TEST_F(ClosureTest, ClosureMultipleForms)
{
    ExprHandle parm = ExpressionFactory::makeNil();

    ExprHandle two  = ExpressionFactory::makeInteger(2);
    ExprHandle zed  = ExpressionFactory::makeCharacter("\\#z");
    ExprHandle forms = ExpressionFactory::makeList(two.get(), zed.get());
    ExprHandle expr = ExpressionFactory::makeClosure(parm.get(),
                                                     forms.get(),
                                                     env);

    expectProcedure(expr, "(lambda () (2 \\#z))");

    ExprHandle args = ExpressionFactory::makeNil();
    ExprHandle final = apply(expr, args);

    expectChar(final, 'z', "\\#z");
}

TEST_F(ClosureTest, ClosureWithArg)
{
    ExprHandle nil  = ExpressionFactory::makeNil();
    ExprHandle argx = ExpressionFactory::makeSymbol("x");
    ExprHandle parm = ExpressionFactory::makeList(argx.get());

    ExprHandle plus = ExpressionFactory::makeSymbol("+");

    ExprHandle form1 = ExpressionFactory::makeList(plus.get(),
                                                   argx.get(),
                                                   argx.get());
    ExprHandle forms = ExpressionFactory::makeList(form1.get());

    ExprHandle expr = ExpressionFactory::makeClosure(parm.get(),
                                                     forms.get(),
                                                     env);

    expectProcedure(expr, "(lambda (x) ((+ x x)))");

    ExprHandle arg3 = ExpressionFactory::makeInteger(3);
    ExprHandle args = ExpressionFactory::makeList(arg3.get());
    try {
        ExprHandle final = apply(expr, args);
        expectInteger(final, 6, "6");
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage();
    }
}

TEST_F(ClosureTest, LambdaBasic)
{
    ExprHandle expr = parseAndEvaluate("(lambda () 2)");
    expectProcedure(expr, "(lambda () (2))");
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
    Env old = env;
    parseAndEvaluate("(define f ())");

    env = env.extend();
    parseAndEvaluate("(define y 5)");
    parseAndEvaluate("(assign f (lambda (x) (* x y)))");
    env = old;

    ExprHandle expr = parseAndEvaluate("(f (+ 1 3))");
    expectInteger(expr, 20, "20");
}

TEST_F(ClosureTest, LambdaFormalsMaskEnv)
{
    parseAndEvaluate("(define x 0.0)");
    parseAndEvaluate("(define f (lambda (x) (/ 1.0 x)))");
    ExprHandle expr = parseAndEvaluate("(f 2)");
    expectFloat(expr, 0.5, "0.5");

    expr = parseAndEvaluate("x");
    expectFloat(expr, 0.0, "0");
}

TEST_F(ClosureTest, LambdaTooFewActuals)
{
    parseAndEvaluate("(define x 0.0)");
    parseAndEvaluate("(define f (lambda (x) (/ 1.0 x)))");
    ExprHandle expr = parseAndEvaluate("(f)");
    expectError(expr);
}

TEST_F(ClosureTest, LambdaTooManyActuals)
{
    parseAndEvaluate("(define x 0.0)");
    parseAndEvaluate("(define f (lambda (x) (/ 1.0 x)))");
    ExprHandle expr = parseAndEvaluate("(f 1 2 3 4)");
    expectError(expr);
}

TEST_F(ClosureTest, LambdaDottedParmListZero)
{
    parseAndEvaluate("(define x 0.0)");
    parseAndEvaluate("(define f (lambda (x . y) y))");
    ExprHandle expr = parseAndEvaluate("(f 1)");
    expectNil(expr);
}

TEST_F(ClosureTest, LambdaDottedParmListOne)
{
    parseAndEvaluate("(define x 0.0)");
    parseAndEvaluate("(define f (lambda (x . y) y))");
    ExprHandle expr = parseAndEvaluate("(f 1 2)");
    expectList(expr, "(2)", 1);
}

TEST_F(ClosureTest, LambdaDottedParmListSeveral)
{
    parseAndEvaluate("(define x 0.0)");
    parseAndEvaluate("(define f (lambda (x . y) y))");
    ExprHandle expr = parseAndEvaluate("(f 1 2 (+ 2 2) #t)");
    expectList(expr, "(2 4 #t)", 3);
}

TEST_F(ClosureTest, LambdaSymbolParmListNone)
{
    parseAndEvaluate("(define x 0.0)");
    parseAndEvaluate("(define f (lambda x x))");
    ExprHandle expr = parseAndEvaluate("(f)");
    expectNil(expr);
}

TEST_F(ClosureTest, LambdaSymbolParmListOne)
{
    parseAndEvaluate("(define f (lambda x x))");
    ExprHandle expr = parseAndEvaluate("(f 5)");
    expectList(expr, "(5)", 1);
}

TEST_F(ClosureTest, LambdaSymbolParmListSeveral)
{
    parseAndEvaluate("(define f (lambda x x))");
    ExprHandle expr = parseAndEvaluate("(f 5 10 15)");
    expectList(expr, "(5 10 15)", 3);
}
