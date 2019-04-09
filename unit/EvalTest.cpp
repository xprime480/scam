
#include "TestBase.hpp"

using namespace std;
using namespace scam;

class EvalTest : public TestBase
{
};

TEST_F(EvalTest, QuoteBasic)
{
    ScamExpr * expr = parseAndEvaluate("(quote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuoteBasicMacro)
{
    ScamExpr * expr = parseAndEvaluate("'(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteBasic)
{
    ScamExpr * expr = parseAndEvaluate("(quasiquote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteBasicMacro)
{
    ScamExpr * expr = parseAndEvaluate("`(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuoteIgnoresUnquote)
{
    ScamExpr * expr = parseAndEvaluate("',(+ 2 2)");
    expectList(expr, "(unquote (+ 2 2))", 2);

    ScamExpr * inner = expr->nthcar(1);
    expectList(inner, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteWithUnquote)
{
    ScamExpr * expr = parseAndEvaluate("`,(+ 2 2)");
    expectInteger(expr, 4, "4");
}

TEST_F(EvalTest, QuasiQuoteWithNestedUnquote)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/eval/nestedunquote.scm");
    expectList(expr, "(+ (+ 3 (+ 99 1 2 3)) 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteSpliceInternal)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/eval/spliceinternal.scm");
    expectList(expr, "(+ x 3 100 2)", 5);
}

TEST_F(EvalTest, QuasiQuoteSpliceOnly)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/eval/spliceonly.scm");
    expectList(expr, "(3 100)", 2);
}

TEST_F(EvalTest, QuasiQuoteSpliceEmpty)
{
    ScamExpr * expr = parseAndEvaluate("`(+ x ,@(list) 2)");
    expectList(expr, "(+ x 2)", 3);
}

TEST_F(EvalTest, MacroLiteralForm)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/eval/macroliteral.scm");
    expectInteger(expr, 2, "2");
}

TEST_F(EvalTest, MacroFormNeedsEvaluation)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/eval/macroeval.scm");
    expectInteger(expr, 7, "7");
}

TEST_F(EvalTest, MacroMyLetTest)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/eval/mylet.scm");
    expectList(expr, "(1 2)", 2);
}

TEST_F(EvalTest, EvalSelfEvaluating)
{
    ScamExpr * expr = parseAndEvaluate("(eval 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(EvalTest, EvalForm)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/eval/evalsattop.scm");
    expectList(expr, "(2 4)", 2);
}

TEST_F(EvalTest, Progn)
{
    ScamExpr * expr = parseAndEvaluate("(progn 1 2 3 (* 5 4) 99)");
    expectInteger(expr, 99, "99");
}

TEST_F(EvalTest, PrognOne)
{
    ScamExpr * expr = parseAndEvaluate("(progn 99)");
    expectInteger(expr, 99, "99");
}

TEST_F(EvalTest, PrognZero)
{
    ScamExpr * expr = parseAndEvaluate("(progn)");
    expectNil(expr);
}

TEST_F(EvalTest, ApplyPlus)
{
    ScamExpr * expr = parseAndEvaluate("(apply * (list 3 33))");
    expectInteger(expr, 99, "99");
}
