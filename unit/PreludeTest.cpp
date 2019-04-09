
#include "TestBase.hpp"

using namespace std;
using namespace scam;

class PreludeTest : public TestBase
{
};

TEST_F(PreludeTest, MaxIntegerTest)
{
    ScamExpr * expr = parseAndEvaluate("(max 123 -123)");
    expectInteger(expr, 123, "123");
}

TEST_F(PreludeTest, MaxFloatTest)
{
    ScamExpr * expr = parseAndEvaluate("(max 42.01 17.5)");
    expectFloat(expr, 42.01, "42.01");
}

TEST_F(PreludeTest, MaxMixedNumericTest)
{
    ScamExpr * expr = parseAndEvaluate("(max -5 -9.999)");
    expectInteger(expr, -5, "-5");
}

TEST_F(PreludeTest, MinIntegerTest)
{
    ScamExpr * expr = parseAndEvaluate("(min 123 -123)");
    expectInteger(expr, -123, "-123");
}

TEST_F(PreludeTest, MinFloatTest)
{
    ScamExpr * expr = parseAndEvaluate("(min 42.01 17.5)");
    expectFloat(expr, 17.5, "17.5");
}

TEST_F(PreludeTest, MinMixedNumericTest)
{
    ScamExpr * expr = parseAndEvaluate("(min -5 -9.999)");
    expectFloat(expr, -9.999, "-9.999");
}

TEST_F(PreludeTest, MapTest)
{
    ScamExpr * expr = parseAndEvaluate("(map integer? (list 1 0.123 \"xx\"))");
    expectList(expr, "(#t #f #f)", 3);
}

TEST_F(PreludeTest, ReduceEmptyList)
{
    ScamExpr * expr = parseAndEvaluate("(reduce (lambda (a b) (+ a b)) 0 '())");
    expectInteger(expr, 0, "0");
}

TEST_F(PreludeTest, ReduceSingleton)
{
    ScamExpr * expr
        = parseAndEvaluate("(reduce (lambda (a b) (+ a b)) 0 '(5))");
    expectInteger(expr, 5, "5");
}

TEST_F(PreludeTest, ReduceMany)
{
    ScamExpr * expr
        = parseAndEvaluate("(reduce (lambda (a b) (+ a b)) 0 '(1 2 3 4 5))");
    expectInteger(expr, 15, "15");
}

TEST_F(PreludeTest, ReduceRespectsInit)
{
    ScamExpr * expr
        = parseAndEvaluate("(reduce (lambda (a b) (+ a b)) -10 '(1 2 3 4 5))");
    expectInteger(expr, 5, "5");
}

TEST_F(PreludeTest, ReduceListOps)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/prelude/reducelist1.scm");
    expectList(expr, "(#f 1 3 2)", 4);
}

TEST_F(PreludeTest, ReduceListOps2)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/prelude/reducelist2.scm");
    expectList(expr, "((#t) (1 3) (2))", 3);
}

TEST_F(PreludeTest, FilterNil)
{
    ScamExpr * expr = parseAndEvaluate("(filter even? '())");
    expectNil(expr);
}

TEST_F(PreludeTest, FilterList)
{
    ScamExpr * expr = parseAndEvaluate("(filter even? '(1 2 3 4 5))");
    expectList(expr, "(2 4)", 2);
}

TEST_F(PreludeTest, EvenTest)
{
    expectTrue("(even? -2)");
    expectTrue("(even? 0)");
    expectTrue("(even? 2)");
    expectTrue("(even? 12398234)");

    expectFalse("(even? -3)");
    expectFalse("(even? 3)");
    expectFalse("(even? 82383)");

    expectFalse("(even? 2.0)");
    expectFalse("(even? #t)");
    expectFalse("(even? \"Silly, strings don't have parity\")");
}

TEST_F(PreludeTest, OddTest)
{
    expectTrue("(odd? -3)");
    expectTrue("(odd? 3)");
    expectTrue("(odd? 82383)");

    expectFalse("(odd? -2)");
    expectFalse("(odd? 0)");
    expectFalse("(odd? 2)");
    expectFalse("(odd? 12398234)");

    expectFalse("(odd? 3.0)");
    expectFalse("(odd? #t)");
    expectFalse("(odd? \"Silly, strings don't have parity\")");
}

TEST_F(PreludeTest, RequireTest)
{
    expectTrue("(require #t)");

    ScamExpr * expr = parseAndEvaluate("(require #f)");
    expectError(expr, "No more choices", false);
}

TEST_F(PreludeTest, MempEmptyList)
{
    expectFalse("(member? 1 (list))");
}

TEST_F(PreludeTest, MempItemFound)
{
    expectTrue("(member? 1 (list 5 4 3 2 1))");
}

TEST_F(PreludeTest, MempItemNotFound)
{
    expectFalse("(member? 99 (list 5 4 3 2 1))");
}

TEST_F(PreludeTest, DistinctNone)
{
    expectTrue("(distinct? (list))");
}

TEST_F(PreludeTest, DistinctOne)
{
    expectTrue("(distinct? (list 1))");
}

TEST_F(PreludeTest, DistinctAllDifferent)
{
    expectTrue("(distinct? (list 1 5 #f 'cat))");
}

TEST_F(PreludeTest, DistinctWithDuplicates)
{
    expectFalse("(distinct? (list 1 5 #f 'cat 5))");
}

TEST_F(PreludeTest, OneofNone)
{
    ScamExpr * expr = parseAndEvaluate("(one-of (list))");
    expectError(expr, "No more choices", false);
}

TEST_F(PreludeTest, OneofOne)
{
    ScamExpr * expr = parseAndEvaluate("(one-of (list 2))");
    expectInteger(expr, 2, "2");
}

TEST_F(PreludeTest, OneofSecondofThree)
{
    ScamExpr * expr = parseAndEvaluate("(one-of (list 2 8 22)) ?");
    expectInteger(expr, 8, "8");
}

TEST_F(PreludeTest, ExcludeNilFromNil)
{
    ScamExpr * expr = parseAndEvaluate("(exclude () ())");
    expectNil(expr);
}

TEST_F(PreludeTest, ExcludeNilFromList)
{
    ScamExpr * expr = parseAndEvaluate("(exclude () (list 1 2 3))");
    expectList(expr, "(1 2 3)", 3);
}

TEST_F(PreludeTest, ExcludeListFromNil)
{
    ScamExpr * expr = parseAndEvaluate("(exclude  (list 1 2 3) ())");
    expectNil(expr);
}

TEST_F(PreludeTest, ExcludeAllFromList)
{
    ScamExpr * expr = parseAndEvaluate("(define x (list 1 2 3)) (exclude x x)");
    expectNil(expr);
}

TEST_F(PreludeTest, ExcludePartial)
{
    ScamExpr * expr = parseAndEvaluate("(define x (list 1 2 3)) (exclude (cdr x) x)");
    expectList(expr, "(1)", 1);
}

TEST_F(PreludeTest, ExcludeNonOverlapping)
{
    ScamExpr * expr = parseAndEvaluate("(exclude (list 1 2) (list 3 4))");
    expectList(expr, "(3 4)", 2);
}

TEST_F(PreludeTest, LengthOfNil)
{
    ScamExpr * expr = parseAndEvaluate("(length '())");
    expectInteger(expr, 0, "0");
}

TEST_F(PreludeTest, LengthOfSingleton)
{
    ScamExpr * expr = parseAndEvaluate("(length '(1))");
    expectInteger(expr, 1, "1");
}

TEST_F(PreludeTest, LengthOfLongerList)
{
    ScamExpr * expr = parseAndEvaluate("(length '(1 2 3 4))");
    expectInteger(expr, 4, "4");
}

TEST_F(PreludeTest, LengthWithNested)
{
    ScamExpr * expr = parseAndEvaluate("(length '(1 2 (hi there) 3 4))");
    expectInteger(expr, 5, "5");
}

TEST_F(PreludeTest, LengthVector)
{
    ScamExpr * expr = parseAndEvaluate("(length [1 2 3])");
    expectInteger(expr, 3, "3");
}

TEST_F(PreludeTest, LengthDict)
{
    ScamExpr * expr = parseAndEvaluate("(length { :a 44 :b \"cat\" })");
    expectInteger(expr, 2, "2");
}

TEST_F(PreludeTest, LengthBadType)
{
    ScamExpr * expr = parseAndEvaluate("(length :abc)");
    expectError(expr);
}

TEST_F(PreludeTest, NthList)
{
    ScamExpr * expr = parseAndEvaluate("(nth 0 '(a b c))");
    expectSymbol(expr, "a");
}

TEST_F(PreludeTest, NthListOutOfBounds)
{
    ScamExpr * expr = parseAndEvaluate("(nth 4 '(a b c))");
    expectError(expr);
}

TEST_F(PreludeTest, NthVector)
{
    ScamExpr * expr = parseAndEvaluate("(nth 0 [1 2 3])");
    expectInteger(expr, 1, "1");
}

TEST_F(PreludeTest, NthVectorOutOfBounds)
{
    ScamExpr * expr = parseAndEvaluate("(nth -50 [1 2 3])");
    expectError(expr);
}

TEST_F(PreludeTest, NthNotIndexible)
{
    ScamExpr * expr = parseAndEvaluate("(nth 0 { :a 333.333 })");
    expectError(expr);
}

TEST_F(PreludeTest, AppendNilToNil)
{
    ScamExpr * expr = parseAndEvaluate("(append '() '())");
    expectNil(expr);
}

TEST_F(PreludeTest, AppendNilToList)
{
    ScamExpr * expr = parseAndEvaluate("(append '() '(1))");
    expectList(expr, "(1)", 1);
}

TEST_F(PreludeTest, AppendListToNil)
{
    ScamExpr * expr = parseAndEvaluate("(append '(1 3) '())");
    expectList(expr, "(1 3)", 2);
}

TEST_F(PreludeTest, AppendListToList)
{
    ScamExpr * expr = parseAndEvaluate("(append '(1 3) '(#t #f))");
    expectList(expr, "(1 3 #t #f)", 4);
}

TEST_F(PreludeTest, CrossNilWithNil)
{
    ScamExpr * expr = parseAndEvaluate("(cross cons '() '())");
    expectNil(expr);
}

TEST_F(PreludeTest, CrossNilWithList)
{
    ScamExpr * expr = parseAndEvaluate("(cross cons '() '((1) (2)))");
    expectNil(expr);
}

TEST_F(PreludeTest, CrossListWithNil)
{
    ScamExpr * expr = parseAndEvaluate("(cross (lambda (a b) (+ a b)) '(1 2 3) '())");
    expectNil(expr);
}

TEST_F(PreludeTest, CrossListWithList)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/prelude/crosstest1.scm");
    expectList(expr, "(2 3 4 3 4 5 4 5 6)", 9);
}

TEST_F(PreludeTest, PowerSetNil)
{
    ScamExpr * expr = parseAndEvaluate("(power-set '())");
    expectList(expr, "(())", 1);
}

TEST_F(PreludeTest, PowerSetSingleton)
{
    ScamExpr * expr = parseAndEvaluate("(power-set '(1))");
    expectList(expr, "(() (1))", 2);
}

TEST_F(PreludeTest, PowerSetList)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/prelude/powerset.scm");
    expectList(expr, "(#t #t #t #t)", 4);
}


TEST_F(PreludeTest, SomeOfNone)
{
    ScamExpr * expr = parseAndEvaluate("(some-of '())");
    expectError(expr, "No more choices", false);
}

TEST_F(PreludeTest, SomeOfOne)
{
    ScamExpr * expr = parseAndEvaluate("(some-of '(1))");
    expectList(expr, "(1)", 1);

    expr = parseAndEvaluate("?");
    expectError(expr, "No more choices", false);
}

TEST_F(PreludeTest, CondNoClauses)
{
    ScamExpr * expr = parseAndEvaluate("(cond ())");
    expectNil(expr);
}

TEST_F(PreludeTest, CondOneTrueClause)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/cond/onetrue.scm");
    expectString(expr, "One");
}

TEST_F(PreludeTest, CondOneFalseClause)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/cond/onefalse.scm");
    expectNil(expr);
}

TEST_F(PreludeTest, CondManyClauses)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/cond/many.scm");
    expectString(expr, "Last");
}
