#include "TestBase.hpp"

using namespace std;
using namespace scam;

class EvalTest : public TestBase
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
    ExprHandle expr = parseAndEvaluateFile("scripts/eval/nestedunquote.scm");
    expectList(expr, "(+ (+ 3 (+ 99 1 2 3)) 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteSpliceInternal)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/eval/spliceinternal.scm");
    expectList(expr, "(+ x 3 100 2)", 5);
}

TEST_F(EvalTest, QuasiQuoteSpliceOnly)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/eval/spliceonly.scm");
    expectList(expr, "(3 100)", 2);
}

TEST_F(EvalTest, QuasiQuoteSpliceEmpty)
{
    ExprHandle expr = parseAndEvaluate("`(+ x ,@(list) 2)");
    expectList(expr, "(+ x 2)", 3);
}

TEST_F(EvalTest, MacroLiteralForm)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/eval/macroliteral.scm");
    expectInteger(expr, 2, "2");
}

TEST_F(EvalTest, MacroFormNeedsEvaluation)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/eval/macroeval.scm");
    expectInteger(expr, 7, "7");
}

TEST_F(EvalTest, MacroMyLetTest)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/eval/mylet.scm");
    expectList(expr, "(1 2)", 2);
}

TEST_F(EvalTest, EvalSelfEvaluating)
{
    ExprHandle expr = parseAndEvaluate("(eval 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(EvalTest, EvalForm)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/eval/evalsattop.scm");
    expectList(expr, "(2 4)", 2);
}

TEST_F(EvalTest, Begin)
{
    ExprHandle expr = parseAndEvaluate("(begin 1 2 3 (* 5 4) 99)");
    expectInteger(expr, 99, "99");
}

TEST_F(EvalTest, BeginOne)
{
    ExprHandle expr = parseAndEvaluate("(begin 99)");
    expectInteger(expr, 99, "99");
}

TEST_F(EvalTest, BeginZero)
{
    ExprHandle expr = parseAndEvaluate("(begin)");
    expectNil(expr);
}

TEST_F(EvalTest, ApplyValid)
{
    ExprHandle expr = parseAndEvaluate("(apply * (list 3 33))");
    expectInteger(expr, 99, "99");
}

TEST_F(EvalTest, ApplyTooManyArgs)
{
    ExprHandle expr = parseAndEvaluate("(apply * 3 33)");
    expectError(expr);
}

TEST_F(EvalTest, ApplyTooFewArgs)
{
    ExprHandle expr = parseAndEvaluate("(apply)");
    expectError(expr);
}

