#include "TestBase.hpp"

#include "expr/SequenceOps.hpp"

using namespace std;
using namespace scam;

class EvalTest : public TestBase
{
};

TEST_F(EvalTest, QuoteBasic)
{
    ScamValue expr = readEval("(quote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuoteBasicMacro)
{
    ScamValue expr = readEval("'(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteBasic)
{
    ScamValue expr = readEval("(quasiquote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteBasicMacro)
{
    ScamValue expr = readEval("`(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuoteIgnoresUnquote)
{
    ScamValue expr = readEval("',(+ 2 2)");
    expectList(expr, "(unquote (+ 2 2))", 2);

    ScamValue inner = nthcar(expr, 1);
    expectList(inner, "(+ 2 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteWithUnquote)
{
    ScamValue expr = readEval("`,(+ 2 2)");
    expectInteger(expr, 4, "4", true);
}

TEST_F(EvalTest, QuasiQuoteWithNestedUnquote)
{
    ScamValue expr = readEvalFile("scripts/eval/nestedunquote.scm");
    expectList(expr, "(+ (+ 3 (+ 99 1 2 3)) 2)", 3);
}

TEST_F(EvalTest, QuasiQuoteSpliceInternal)
{
    ScamValue expr = readEvalFile("scripts/eval/spliceinternal.scm");
    expectList(expr, "(+ x 3 100 2)", 5);
}

TEST_F(EvalTest, QuasiQuoteSpliceOnly)
{
    ScamValue expr = readEvalFile("scripts/eval/spliceonly.scm");
    expectList(expr, "(3 100)", 2);
}

TEST_F(EvalTest, QuasiQuoteSpliceEmpty)
{
    ScamValue expr = readEval("`(+ x ,@(list) 2)");
    expectList(expr, "(+ x 2)", 3);
}

TEST_F(EvalTest, MacroLiteralForm)
{
    ScamValue expr = readEvalFile("scripts/eval/macroliteral.scm");
    expectInteger(expr, 2, "2", true);
}

TEST_F(EvalTest, MacroFormNeedsEvaluation)
{
    ScamValue expr = readEvalFile("scripts/eval/macroeval.scm");
    expectInteger(expr, 7, "7", true);
}

TEST_F(EvalTest, MacroMyLetTest)
{
    ScamValue expr = readEvalFile("scripts/eval/mylet.scm");
    expectList(expr, "(1 2)", 2);
}

TEST_F(EvalTest, EvalSelfEvaluating)
{
    ScamValue expr = readEval("(eval 2)");
    expectInteger(expr, 2, "2", true);
}

TEST_F(EvalTest, EvalForm)
{
    ScamValue expr = readEvalFile("scripts/eval/evalsattop.scm");
    expectList(expr, "(2 4)", 2);
}

TEST_F(EvalTest, Begin)
{
    ScamValue expr = readEval("(begin 1 2 3 (* 5 4) 99)");
    expectInteger(expr, 99, "99", true);
}

TEST_F(EvalTest, BeginOne)
{
    ScamValue expr = readEval("(begin 99)");
    expectInteger(expr, 99, "99", true);
}

TEST_F(EvalTest, BeginZero)
{
    ScamValue expr = readEval("(begin)");
    expectNull(expr);
}

TEST_F(EvalTest, ApplyValid)
{
    ScamValue expr = readEval("(apply * (list 3 33))");
    expectInteger(expr, 99, "99", true);
}

TEST_F(EvalTest, ApplyTooManyArgs)
{
    ScamValue expr = readEval("(apply * 3 33)");
    expectError(expr);
}

TEST_F(EvalTest, ApplyTooFewArgs)
{
    ScamValue expr = readEval("(apply)");
    expectError(expr);
}

