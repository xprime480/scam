
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
    ExprHandle nil  = ExpressionFactory::makeNil();
    ExprHandle two  = ExpressionFactory::makeInteger(2);
    ExprHandle forms = ExpressionFactory::makeCons(two.get(), nil.get());
    ExprHandle expr = ExpressionFactory::makeClosure(parm.get(), forms.get(), env);

    expectProcedure(expr, "(proc () (2))");

    ExprHandle args = ExpressionFactory::makeNil();
    ExprHandle final = apply(expr, args);

    expectInteger(final, 2, "2");
}

TEST_F(ClosureTest, ClosureMultipleForms)
{
    ExprHandle parm = ExpressionFactory::makeNil();
    ExprHandle nil  = ExpressionFactory::makeNil();
    ExprHandle two  = ExpressionFactory::makeInteger(2);
    ExprHandle zed  = ExpressionFactory::makeCharacter("\\#z");
    ExprHandle cdr  = ExpressionFactory::makeCons(zed.get(), nil.get());
    ExprHandle forms = ExpressionFactory::makeCons(two.get(), cdr.get());
    ExprHandle expr = ExpressionFactory::makeClosure(parm.get(),
                                                     forms.get(),
                                                     env);

    expectProcedure(expr, "(proc () (2 \\#z))");

    ExprHandle args = ExpressionFactory::makeNil();
    ExprHandle final = apply(expr, args);

    expectChar(final, 'z', "\\#z");
}

TEST_F(ClosureTest, ClosureWithArg)
{
    ExprHandle nil  = ExpressionFactory::makeNil();
    ExprHandle argx = ExpressionFactory::makeSymbol("x");
    ExprHandle parm = ExpressionFactory::makeCons(argx.get(), nil.get());

    ExprHandle plus = ExpressionFactory::makeSymbol("+");

    ExprHandle forms = ExpressionFactory::makeCons(argx.get(), nil.get());
    forms = ExpressionFactory::makeCons(argx.get(), forms.get());
    forms = ExpressionFactory::makeCons(plus.get(), forms.get());
    forms = ExpressionFactory::makeCons(forms.get(), nil.get());

    ExprHandle expr = ExpressionFactory::makeClosure(parm.get(),
                                                     forms.get(),
                                                     env);

    expectProcedure(expr, "(proc (x) ((+ x x)))");

    ExprHandle arg3 = ExpressionFactory::makeInteger(3);
    ExprHandle args = ExpressionFactory::makeCons(arg3.get(), nil.get());
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
    expectProcedure(expr, "(proc () (2))");
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
