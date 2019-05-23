#include "TestBase.hpp"

using namespace std;
using namespace scam;

class EvalTest : public TestBase
{
};

TEST_F(EvalTest, QuoteBasic)
{
    ScamValue expr = parseAndEvaluate("(quote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuoteBasicMacro)
{
    ScamValue expr = parseAndEvaluate("'(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteBasic)
{
    ScamValue expr = parseAndEvaluate("(quasiquote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteBasicMacro)
{
    ScamValue expr = parseAndEvaluate("`(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuoteIgnoresUnquote)
{
    ScamValue expr = parseAndEvaluate("',(+ 2 2)");
    expectList(expr, "(unquote (+ 2 2))", 2);

    ScamValue inner = expr->nthcar(1);
    expectList(inner, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteWithUnquote)
{
    ScamValue expr = parseAndEvaluate("`,(+ 2 2)");
    expectInteger(expr, 4, "4", true);
}

TEST_F(EvalTest, QuasiQuoteWithNestedUnquote)
{
    ScamValue expr = parseAndEvaluateFile("scripts/eval/nestedunquote.scm");
    expectList(expr, "(+ (+ 3 (+ 99 1 2 3)) 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteSpliceInternal)
{
    ScamValue expr = parseAndEvaluateFile("scripts/eval/spliceinternal.scm");
    expectList(expr, "(+ x 3 100 2)", 5);
}

TEST_F(EvalTest, QuasiQuoteSpliceOnly)
{
    ScamValue expr = parseAndEvaluateFile("scripts/eval/spliceonly.scm");
    expectList(expr, "(3 100)", 2);
}

TEST_F(EvalTest, QuasiQuoteSpliceEmpty)
{
    ScamValue expr = parseAndEvaluate("`(+ x ,@(list) 2)");
    expectList(expr, "(+ x 2)", 3);
}

TEST_F(EvalTest, MacroLiteralForm)
{
    ScamValue expr = parseAndEvaluateFile("scripts/eval/macroliteral.scm");
    expectInteger(expr, 2, "2", true);
}

TEST_F(EvalTest, MacroFormNeedsEvaluation)
{
    ScamValue expr = parseAndEvaluateFile("scripts/eval/macroeval.scm");
    expectInteger(expr, 7, "7", true);
}

TEST_F(EvalTest, MacroMyLetTest)
{
    ScamValue expr = parseAndEvaluateFile("scripts/eval/mylet.scm");
    expectList(expr, "(1 2)", 2);
}

TEST_F(EvalTest, EvalSelfEvaluating)
{
    ScamValue expr = parseAndEvaluate("(eval 2)");
    expectInteger(expr, 2, "2", true);
}

TEST_F(EvalTest, EvalForm)
{
    ScamValue expr = parseAndEvaluateFile("scripts/eval/evalsattop.scm");
    expectList(expr, "(2 4)", 2);
}

TEST_F(EvalTest, Begin)
{
    ScamValue expr = parseAndEvaluate("(begin 1 2 3 (* 5 4) 99)");
    expectInteger(expr, 99, "99", true);
}

TEST_F(EvalTest, BeginOne)
{
    ScamValue expr = parseAndEvaluate("(begin 99)");
    expectInteger(expr, 99, "99", true);
}

TEST_F(EvalTest, BeginZero)
{
    ScamValue expr = parseAndEvaluate("(begin)");
    expectNil(expr);
}

TEST_F(EvalTest, ApplyValid)
{
    ScamValue expr = parseAndEvaluate("(apply * (list 3 33))");
    expectInteger(expr, 99, "99", true);
}

TEST_F(EvalTest, ApplyTooManyArgs)
{
    ScamValue expr = parseAndEvaluate("(apply * 3 33)");
    expectError(expr);
}

TEST_F(EvalTest, ApplyTooFewArgs)
{
    ScamValue expr = parseAndEvaluate("(apply)");
    expectError(expr);
}

