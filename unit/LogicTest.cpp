
#include "TestBase.hpp"

#include "ScamException.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace std;
using namespace scam;

class LogicTest : public TestBase
{
};

TEST_F(LogicTest, IfTrue)
{
    ScamExpr * expr = parseAndEvaluate("(if #t +1 -1)");
    expectInteger(expr, 1, "1");
}

TEST_F(LogicTest, IfFalse)
{
    ScamExpr * expr = parseAndEvaluate("(if #f +1 -1)");
    expectInteger(expr, -1, "-1");
}

TEST_F(LogicTest, IfFalseNoElse)
{
    ScamExpr * expr = parseAndEvaluate("(if #f +1)");
    expectNil(expr);
}

TEST_F(LogicTest, IfDoesntEvaluateUnusedClause)
{
    ScamExpr * expr = parseAndEvaluate("(if #t (* 2 3) (/ 1 0))");
    expectInteger(expr, 6, "6");
}

TEST_F(LogicTest, IfTestError)
{
    ScamExpr * expr = parseAndEvaluate("(if (/ 1 0) (* 2 3) (/ 1 0))");
    expectError(expr);
}

TEST_F(LogicTest, IfThenError)
{
    ScamExpr * expr = parseAndEvaluate("(if \"strings are true\" (/ 1 0))");
    expectError(expr);
}

TEST_F(LogicTest, IfTooFewClauses)
{
    ScamExpr * expr = parseAndEvaluate("(if #t)");
    expectError(expr);
}

TEST_F(LogicTest, IfTooManyClauses)
{
    ScamExpr * expr = parseAndEvaluate("(if #t 1 2 3 4 5 ())");
    expectError(expr);
}

TEST_F(LogicTest, AndZeroForms)
{
    expectTrue("(and)");
}

TEST_F(LogicTest, AndOneTrue)
{
    ScamExpr * expr = parseAndEvaluate("(and 3)");
    expectInteger(expr, 3, "3");
}

TEST_F(LogicTest, AndOneFalse)
{
    expectFalse("(and #f)");
}

TEST_F(LogicTest, AndManyTrue)
{
    ScamExpr * expr = parseAndEvaluate("(and #t #t 3)");
    expectInteger(expr, 3, "3");
}

TEST_F(LogicTest, AndShortCircuits)
{
    expectFalse("(and #f (/ 1 0))");
}

TEST_F(LogicTest, AndComplex1)
{
    ScamExpr * expr = parseAndEvaluate("(and 2 (and 3 4))");
    expectInteger(expr, 4, "4");
}

TEST_F(LogicTest, AndComplex2)
{
    ScamExpr * expr = parseAndEvaluate("(and (and 3 4) 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(LogicTest, OrZeroForms)
{
    expectFalse("(or)");
}

TEST_F(LogicTest, OrOneTrue)
{
    ScamExpr * expr = parseAndEvaluate("(or 3)");
    expectInteger(expr, 3, "3");
}

TEST_F(LogicTest, OrOneFalse)
{
    expectFalse("(or #f)");
}

TEST_F(LogicTest, OrManyTrue)
{
    expectTrue("(or #t #t 3)");
}

TEST_F(LogicTest, OrShortCircuits)
{
    expectTrue("(or #t (/ 1 0))");
}

TEST_F(LogicTest, OrComplex1)
{
    ScamExpr * expr = parseAndEvaluate("(or #f (or 3 4))");
    expectInteger(expr, 3, "3");
}

TEST_F(LogicTest, OrComplex2)
{
    ScamExpr * expr = parseAndEvaluate("(or (or #f #f) 2)");
    expectInteger(expr, 2, "2");
}

TEST_F(LogicTest, NotZeroForms)
{
    ScamExpr * expr = parseAndEvaluate("(not)");
    expectError(expr);
}

TEST_F(LogicTest, NotOneTrue)
{
    expectFalse("(not 3)");
}

TEST_F(LogicTest, NotOneFalse)
{
    expectTrue("(not #f)");
}

TEST_F(LogicTest, NotManyTrue)
{
    ScamExpr * expr = parseAndEvaluate("(not #t #t 3)");
    expectError(expr);
}

TEST_F(LogicTest, XorTest)
{
    expectTrue("(xor #t #f)");
    expectTrue("(xor #f #t)");
    expectFalse("(xor #t #t)");
    expectFalse("(xor #f #f)");
}

TEST_F(LogicTest, SubstituteSymbol)
{
    expectTrue("(eq? (substitute :X { :X 23 }) 23)");
}

TEST_F(LogicTest, SubstituteInCons)
{
    expectTrue("(eq? (substitute '(:X 99) { :X 23 }) '(23 99))");
}

TEST_F(LogicTest, SubstituteInVector)
{
    expectTrue("(eq? (substitute [:X 99] { :X 23 }) [23 99])");
}

TEST_F(LogicTest, SubstituteMultiple)
{
    expectTrue("(eq? (substitute [:X :Y :X] { :X 23 :Y cat }) [23 'cat 23])");
}

TEST_F(LogicTest, SubstituteMissing)
{
    ScamExpr * expr = parseAndEvaluate("(substitute :X {})");
    expectError(expr);
}

TEST_F(LogicTest, InstantiateNothing)
{
    ScamExpr * expr = parseAndEvaluate("(instantiate '())");
    expectNil(expr);
}

TEST_F(LogicTest, InstantiateNoKeywords)
{
    ScamExpr * expr = parseAndEvaluate("(instantiate \"test\")");
    expectString(expr, "test");
}

TEST_F(LogicTest, InstantiateKeyword)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/logic/simplekeyword.scm");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, InstantiateConsSingleKeyword)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/logic/consonekeyword.scm");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, InstantiateConsWithDups)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/logic/conswithdups.scm");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, InstantiateConsTwoKeywords)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/logic/constwokeywords.scm");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, InstantiateVector)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/logic/vector.scm");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, InstantiateDict)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/logic/dict.scm");
    expectBoolean(expr, true, "#t");
}

TEST_F(LogicTest, InstantiateSameExprTwice)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/logic/sametwice.scm");
    expectBoolean(expr, true, "#t");
}
