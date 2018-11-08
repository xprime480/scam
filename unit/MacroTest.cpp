
#include "ExpressionTestBase.hpp"

#include "ScamException.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace std;
using namespace scam;

class MacroTest : public ExpressionTestBase
{
};

TEST_F(MacroTest, QuoteBasic)
{
    ExprHandle expr = parseAndEvaluate("(quote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(MacroTest, QuoteBasicMacro)
{
    ExprHandle expr = parseAndEvaluate("'(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(MacroTest, QuasiQuoteBasic)
{
    ExprHandle expr = parseAndEvaluate("(quasiquote (+ 2 2))");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(MacroTest, QuasiQuoteBasicMacro)
{
    ExprHandle expr = parseAndEvaluate("`(+ 2 2)");
    expectList(expr, "(+ 2 2)", 3);
}

TEST_F(MacroTest, QuoteIgnoresUnquote)
{
    ExprHandle expr = parseAndEvaluate("',(+ 2 2)");
    expectList(expr, "(unquote (+ 2 2))", 2);

    ExprHandle inner = expr->nthcar(1);
    expectList(inner, "(+ 2 2)", 3);
}

TEST_F(MacroTest, QuasiQuoteWithUnquote)
{
    ExprHandle expr = parseAndEvaluate("`,(+ 2 2)");
    expectInteger(expr, 4, "4");
}

TEST_F(MacroTest, QuasiQuoteWithNestedUnquote)
{
    parseAndEvaluate("(define x 99)");
    ExprHandle expr = parseAndEvaluate("`(+ (+ 3 (+ ,x 1 2 3)) 2)");
    expectList(expr, "(+ (+ 3 (+ 99 1 2 3)) 2)", 3);
}

TEST_F(MacroTest, QuasiQuoteSpliceInternal)
{
    parseAndEvaluate("(define x 99)");
    ExprHandle expr = parseAndEvaluate("`(+ x ,@(list 3 (+ x 1)) 2)");
    expectList(expr, "(+ x 3 100 2)", 5);
}

TEST_F(MacroTest, QuasiQuoteSpliceOnly)
{
    parseAndEvaluate("(define x 99)");
    ExprHandle expr = parseAndEvaluate("`(,@(list 3 (+ x 1)))");
    expectList(expr, "(3 100)", 2);
}

TEST_F(MacroTest, QuasiQuoteSpliceEmpty)
{
    parseAndEvaluate("(define x 99)");
    ExprHandle expr = parseAndEvaluate("`(+ x ,@(list) 2)");
    expectList(expr, "(+ x 2)", 3);
}

TEST_F(MacroTest, MacroLiteralForm)
{
    parseAndEvaluate("(define test (macro () 2))");
    ExprHandle expr = parseAndEvaluate("(test)");
    expectInteger(expr, 2, "2");
}

TEST_F(MacroTest, MacroFormNeedsEvaluation)
{
    parseAndEvaluate("(define var 5)");
    parseAndEvaluate("(define test (macro (x) `(+ ,x 2)))");
    ExprHandle expr = parseAndEvaluate("(test var)");
    expectInteger(expr, 7, "7");
}

TEST_F(MacroTest, MacroMyLetTest)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/macro/mylet.scm");
    expectList(expr, "(1 2)", 2);
}
