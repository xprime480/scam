
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class EvalTest : public ExpressionTestBase
{
};

TEST_F(EvalTest, QuoteBasic)
{
    ExprHandle expr = parseAndEvaluate("(quote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuoteBasicMacro)
{
    ExprHandle expr = parseAndEvaluate("'(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteBasic)
{
    ExprHandle expr = parseAndEvaluate("(quasiquote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteBasicMacro)
{
    ExprHandle expr = parseAndEvaluate("`(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuoteIgnoresUnquote)
{
    ExprHandle expr = parseAndEvaluate("',(+ 2 2)");
    expectList(expr, "(unquote (+ 2 2))", 2);

    ExprHandle inner = expr->nthcar(1);
    expectList(inner, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteWithUnquote)
{
    ExprHandle expr = parseAndEvaluate("`,(+ 2 2)");
    expectInteger(expr, 4, "4");
}

TEST_F(EvalTest, QuasiQuoteWithNestedUnquote)
{
    parseAndEvaluate("(define x 99)");
    ExprHandle expr = parseAndEvaluate("`(+ (+ 3 (+ ,x 1 2 3)) 2)");
    expectList(expr, "(+ (+ 3 (+ 99 1 2 3)) 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteSpliceInternal)
{
    parseAndEvaluate("(define x 99)");
    ExprHandle expr = parseAndEvaluate("`(+ x ,@(list 3 (+ x 1)) 2)");
    expectList(expr, "(+ x 3 100 2)", 5);
}

TEST_F(EvalTest, QuasiQuoteSpliceOnly)
{
    parseAndEvaluate("(define x 99)");
    ExprHandle expr = parseAndEvaluate("`(,@(list 3 (+ x 1)))");
    expectList(expr, "(3 100)", 2);
}

TEST_F(EvalTest, QuasiQuoteSpliceEmpty)
{
    parseAndEvaluate("(define x 99)");
    ExprHandle expr = parseAndEvaluate("`(+ x ,@(list) 2)");
    expectList(expr, "(+ x 2)", 3);
}

TEST_F(EvalTest, MacroLiteralForm)
{
    parseAndEvaluate("(define test (macro () 2))");
    ExprHandle expr = parseAndEvaluate("(test)");
    expectInteger(expr, 2, "2");
}

TEST_F(EvalTest, MacroFormNeedsEvaluation)
{
    parseAndEvaluate("(define var 5)");
    parseAndEvaluate("(define test (macro (x) `(+ ,x 2)))");
    ExprHandle expr = parseAndEvaluate("(test var)");
    expectInteger(expr, 7, "7");
}

TEST_F(EvalTest, MacroMyLetTest)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/macro/mylet.scm");
    expectList(expr, "(1 2)", 2);
}

TEST_F(EvalTest, EvalSelfEvaluating)
{
    ExprHandle expr = parseAndEvaluate("(eval 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(EvalTest, EvalForm)
{
    parseAndEvaluate("(define x 1)");
    env = env.extend();
    parseAndEvaluate("(define x 2)");

    ExprHandle expr = parseAndEvaluate("x");
    expectInteger(expr, 2, "2");

    expr = parseAndEvaluate("(eval '(+ x 3))");
    expectInteger(expr, 4, "4");
}
