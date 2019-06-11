#include "TestBase.hpp"

#include <sstream>

using namespace std;
using namespace scam;

class PredicateTest : public TestBase
{
};

TEST_F(PredicateTest, NilTestZeroArg)
{
    ScamValue expr = parseAndEvaluate("(nil?)");
    expectError(expr);
}

TEST_F(PredicateTest, NilTestExtraArg)
{
    ScamValue expr = parseAndEvaluate("(nil? 1 2 3)");
    expectError(expr);
}

TEST_F(PredicateTest, NilTestNil)
{
    expectTrue("(nil? ())");
}

TEST_F(PredicateTest, NilTestNotNil)
{
    expectFalse("(nil? 2)");
}

TEST_F(PredicateTest, ListTestNil)
{
    expectTrue("(list? ())");
}

TEST_F(PredicateTest, ListTestList)
{
    expectTrue("(list? (list 1 2 3))");
}

TEST_F(PredicateTest, ListTestCons)
{
    expectFalse("(list? (cons 'a 'b))");
}

TEST_F(PredicateTest, ListTestNotList)
{
    expectFalse("(list? 2)");
}

TEST_F(PredicateTest, VectorTestVector)
{
    expectTrue("(vector? #(1 2 3))");
}

TEST_F(PredicateTest, VectorTestList)
{
    expectFalse("(vector? (list 'a 'b))");
}

TEST_F(PredicateTest, VectorTestNotVector)
{
    expectFalse("(vector? 2)");
}

TEST_F(PredicateTest, CharTestChar)
{
    expectTrue("(char? #\\a)");
}

TEST_F(PredicateTest, CharTestNotChar)
{
    expectFalse("(char? 12.34)");
}

TEST_F(PredicateTest, StringTestEmptyString)
{
    expectTrue("(string? \"\")");
}

TEST_F(PredicateTest, StringTestNonEmptyString)
{
    expectTrue("(string? \"Hello World\")");
}

TEST_F(PredicateTest, StringTestNotString)
{
    expectFalse("(string? ())");
}

TEST_F(PredicateTest, SymbolTestSymbol)
{
    expectTrue("(symbol? (quote a-symbol))");
}

TEST_F(PredicateTest, SymbolTestNotSymbol)
{
    expectFalse("(symbol? (lambda () 2))");
}

TEST_F(PredicateTest, NumericTestReal)
{
    expectTrue("(numeric? 1.5)");
}

TEST_F(PredicateTest, NumericTestInteger)
{
    expectTrue("(numeric? 17)");
}

TEST_F(PredicateTest, NumericTestNotNumeric)
{
    expectFalse("(numeric? ())");
}

TEST_F(PredicateTest, ComplexTestReal)
{
    expectTrue("(complex? 1.5)");
}

TEST_F(PredicateTest, ComplexTestInteger)
{
    expectTrue("(complex? 17)");
}

TEST_F(PredicateTest, ComplexTestNotNumeric)
{
    expectFalse("(complex? ())");
}

TEST_F(PredicateTest, RealTestReal)
{
    expectTrue("(real? 1.5)");
}

TEST_F(PredicateTest, RealTestInteger)
{
    expectTrue("(real? 17)");
}

TEST_F(PredicateTest, RealTestNotNumeric)
{
    expectFalse("(real? ())");
}

TEST_F(PredicateTest, RationalTestReal)
{
    expectTrue("(rational? 1.5)");
}

TEST_F(PredicateTest, RationalTestInteger)
{
    expectTrue("(rational? 17)");
}

TEST_F(PredicateTest, RationalTestNotNumeric)
{
    expectFalse("(rational? ())");
}

TEST_F(PredicateTest, IntegerTestReal)
{
    expectFalse("(integer? 1.5)");
}

TEST_F(PredicateTest, IntegerTestInteger)
{
    expectTrue("(integer? 17)");
}

TEST_F(PredicateTest, IntegerTestNotNumeric)
{
    expectFalse("(integer? ())");
}

TEST_F(PredicateTest, ProcTestProc)
{
    expectTrue("(proc? (lambda (x) 2))");
}

TEST_F(PredicateTest, ProcTestClass)
{
    expectTrue("(proc? (make-class Root ()))");
}

TEST_F(PredicateTest, ProcTestInstance)
{
    expectTrue("(proc? (load \"scripts/class/trivial2.scm\"))");
}

TEST_F(PredicateTest, ProcTestNotProc)
{
    expectFalse("(proc? ())");
}

TEST_F(PredicateTest, ClassTestClass)
{
    expectTrue("(class? (make-class Root ()))");
}

TEST_F(PredicateTest, ClassTestNotClass)
{
    expectFalse("(class? ())");
}

TEST_F(PredicateTest, InstanceTestInstance)
{
    expectTrue("(instance? (load \"scripts/class/trivial2.scm\"))");
}

TEST_F(PredicateTest, InstanceTestNotInstance)
{
    expectFalse("(instance? ())");
}

TEST_F(PredicateTest, KeywordTestKeyword)
{
    expectTrue("(keyword? :yesiam)");
}

TEST_F(PredicateTest, KeywordTestNotKeyword)
{
    expectFalse("(keyword? 3)");
}

TEST_F(PredicateTest, ExactExactInteger)
{
    expectTrue("(exact? 3)");
}

TEST_F(PredicateTest, ExactInexactReal)
{
    expectFalse("(exact? 3.9123)");
}
