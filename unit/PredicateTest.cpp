
#include "ExpressionTestBase.hpp"

#include <sstream>

using namespace std;
using namespace scam;

class PredicateTest : public ExpressionTestBase
{
};

TEST_F(PredicateTest, NilTestZeroArg)
{
    ExprHandle expr = parseAndEvaluate("(nil?)");
    expectError(expr);
}

TEST_F(PredicateTest, NilTestExtraArg)
{
    ExprHandle expr = parseAndEvaluate("(nil? 1 2 3)");
    expectError(expr);
}

TEST_F(PredicateTest, NilTestNil)
{
    ExprHandle expr = parseAndEvaluate("(nil? ())");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, NilTestNotNil)
{
    ExprHandle expr = parseAndEvaluate("(nil? 2)");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, ConsTestCons)
{
    ExprHandle expr = parseAndEvaluate("(cons? (cons 1 2))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, ConsTestList)
{
    ExprHandle expr = parseAndEvaluate("(cons? (list 1 2 3))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, ConsTestNotCons)
{
    ExprHandle expr = parseAndEvaluate("(cons? 2)");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, ListTestNil)
{
    ExprHandle expr = parseAndEvaluate("(list? ())");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, ListTestList)
{
    ExprHandle expr = parseAndEvaluate("(list? (list 1 2 3))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, ListTestCons)
{
    ExprHandle expr = parseAndEvaluate("(list? (cons 'a 'b))");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, ListTestNotList)
{
    ExprHandle expr = parseAndEvaluate("(list? 2)");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, VectorTestVector)
{
    ExprHandle expr = parseAndEvaluate("(vector? [1 2 3])");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, VectorTestList)
{
    ExprHandle expr = parseAndEvaluate("(vector? (list 'a 'b))");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, VectorTestNotVector)
{
    ExprHandle expr = parseAndEvaluate("(vector? 2)");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, BooleanTestBoolean)
{
    ExprHandle expr = parseAndEvaluate("(bool? (> 2 0))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, BooleanTestNotBoolean)
{
    ExprHandle expr = parseAndEvaluate("(bool? '(> 2 0))");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, CharTestChar)
{
    ExprHandle expr = parseAndEvaluate("(char? #\\a)");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, CharTestNotChar)
{
    ExprHandle expr = parseAndEvaluate("(bool? 12.34)");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, StringTestEmptyString)
{
    ExprHandle expr = parseAndEvaluate("(string? \"\")");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, StringTestNonEmptyString)
{
    ExprHandle expr = parseAndEvaluate("(string? \"Hello World\")");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, StringTestNotString)
{
    ExprHandle expr = parseAndEvaluate("(string? ())");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, SymbolTestSymbol)
{
    ExprHandle expr = parseAndEvaluate("(symbol? (quote a-symbol))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, SymbolTestNotSymbol)
{
    ExprHandle expr = parseAndEvaluate("(symbol? (lambda () 2))");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, NumericTestFloat)
{
    ExprHandle expr = parseAndEvaluate("(numeric? 1.5)");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, NumericTestInteger)
{
    ExprHandle expr = parseAndEvaluate("(numeric? 17)");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, NumericTestNotNumeric)
{
    ExprHandle expr = parseAndEvaluate("(numeric? ())");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, FloatTestFloat)
{
    ExprHandle expr = parseAndEvaluate("(float? 1.5)");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, FloatTestInteger)
{
    ExprHandle expr = parseAndEvaluate("(float? 17)");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, FloatTestNotNumeric)
{
    ExprHandle expr = parseAndEvaluate("(float? ())");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, IntegerTestFloat)
{
    ExprHandle expr = parseAndEvaluate("(integer? 1.5)");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, IntegerTestInteger)
{
    ExprHandle expr = parseAndEvaluate("(integer? 17)");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, IntegerTestNotNumeric)
{
    ExprHandle expr = parseAndEvaluate("(integer? ())");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, ProcTestProc)
{
    ExprHandle expr = parseAndEvaluate("(proc? (lambda (x) 2))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, ProcTestClass)
{
    ExprHandle expr = parseAndEvaluate("(proc? (make-class Root () ()))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, ProcTestInstance)
{
    ExprHandle expr = parseAndEvaluate("(proc? (load \"scripts/class/trivial2.scm\"))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, ProcTestNotProc)
{
    ExprHandle expr = parseAndEvaluate("(proc? ())");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, ClassTestClass)
{
    ExprHandle expr = parseAndEvaluate("(class? (make-class Root () ()))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, ClassTestNotClass)
{
    ExprHandle expr = parseAndEvaluate("(class? ())");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, InstanceTestInstance)
{
    ExprHandle expr = parseAndEvaluate("(instance? (load \"scripts/class/trivial2.scm\"))");
    booleanTest(expr, true, "#t");
}

TEST_F(PredicateTest, InstanceTestNotInstance)
{
    ExprHandle expr = parseAndEvaluate("(instance? ())");
    booleanTest(expr, false, "#f");
}

TEST_F(PredicateTest, KeywordTestKeyword)
{
    expectTrue("(keyword? :yesiam)");
}

TEST_F(PredicateTest, KeywordTestNotKeyword)
{
    expectFalse("(keyword? 3)");
}
