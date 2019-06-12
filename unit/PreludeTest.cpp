#include "TestBase.hpp"

#include "expr/ScamToInternal.hpp"

using namespace std;
using namespace scam;

class PreludeTest : public TestBase
{
};

TEST_F(PreludeTest, MaxIntegerTest)
{
    ScamValue expr = parseAndEvaluate("(max 123 -123)");
    expectInteger(expr, 123, "123", true);
}

TEST_F(PreludeTest, MaxRealTest)
{
    ScamValue expr = parseAndEvaluate("(max 42.01 17.5)");
    RationalPair value { 4201, 100 };
    expectRational(expr, value, "4201/100", false);
}

TEST_F(PreludeTest, MaxMixedNumericTest)
{
    ScamValue expr = parseAndEvaluate("(max -5 -9.999)");
    expectInteger(expr, -5, "-5", true);
}

TEST_F(PreludeTest, MinIntegerTest)
{
    ScamValue expr = parseAndEvaluate("(min 123 -123)");
    expectInteger(expr, -123, "-123", true);
}

TEST_F(PreludeTest, MinRealTest)
{
    ScamValue expr = parseAndEvaluate("(min 42.01 17.5)");
    RationalPair value { 35, 2 };
    expectRational(expr, value, "35/2", false);
}

TEST_F(PreludeTest, MinMixedNumericTest)
{
    ScamValue expr = parseAndEvaluate("(min -5 -9.999)");
    RationalPair value { -9999, 1000 };
    expectRational(expr, value, "-9999/1000", false);
}

TEST_F(PreludeTest, MapTest)
{
    ScamValue expr =
        parseAndEvaluate("(map integer? (list 1 0.123 \"xx\"))");
    expectList(expr, "(#t #f #f)", 3);
}

TEST_F(PreludeTest, ReduceEmptyList)
{
    ScamValue expr =
        parseAndEvaluate("(reduce (lambda (a b) (+ a b)) 0 '())");
    expectInteger(expr, 0, "0", true);
}

TEST_F(PreludeTest, ReduceSingleton)
{
    ScamValue expr
        = parseAndEvaluate("(reduce (lambda (a b) (+ a b)) 0 '(5))");
    expectInteger(expr, 5, "5", true);
}

TEST_F(PreludeTest, ReduceMany)
{
    ScamValue expr
        = parseAndEvaluate("(reduce (lambda (a b) (+ a b)) 0 '(1 2 3 4 5))");
    expectInteger(expr, 15, "15", true);
}

TEST_F(PreludeTest, ReduceRespectsInit)
{
    ScamValue expr
        = parseAndEvaluate("(reduce (lambda (a b) (+ a b)) -10 '(1 2 3 4 5))");
    expectInteger(expr, 5, "5", true);
}

TEST_F(PreludeTest, ReduceListOps)
{
    ScamValue expr =
        parseAndEvaluateFile("scripts/prelude/reducelist1.scm");
    expectList(expr, "(#f 1 3 2)", 4);
}

TEST_F(PreludeTest, ReduceListOps2)
{
    ScamValue expr =
        parseAndEvaluateFile("scripts/prelude/reducelist2.scm");
    expectList(expr, "((#t) (1 3) (2))", 3);
}

TEST_F(PreludeTest, FilterNil)
{
    ScamValue expr = parseAndEvaluate("(filter even? '())");
    expectNull(expr);
}

TEST_F(PreludeTest, FilterList)
{
    ScamValue expr = parseAndEvaluate("(filter even? '(1 2 3 4 5))");
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

    expectTrue("(even? 2.0)");
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

    expectTrue("(odd? 3.0)");
    expectFalse("(odd? #t)");
    expectFalse("(odd? \"Silly, strings don't have parity\")");
}

TEST_F(PreludeTest, RequireTest)
{
    expectTrue("(require #t)");
    ScamValue expr = parseAndEvaluate("(require #f)");
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
    ScamValue expr = parseAndEvaluate("(one-of (list))");
    expectError(expr, "No more choices", false);
}

TEST_F(PreludeTest, OneofOne)
{
    ScamValue expr = parseAndEvaluate("(one-of (list 2))");
    expectInteger(expr, 2, "2", true);
}

TEST_F(PreludeTest, OneofSecondofThree)
{
    ScamValue expr = parseAndEvaluate("(one-of (list 2 8 22)) ?");
    expectInteger(expr, 8, "8", true);
}

TEST_F(PreludeTest, ExcludeNilFromNil)
{
    ScamValue expr = parseAndEvaluate("(exclude () ())");
    expectNull(expr);
}

TEST_F(PreludeTest, ExcludeNilFromList)
{
    ScamValue expr = parseAndEvaluate("(exclude () (list 1 2 3))");
    expectList(expr, "(1 2 3)", 3);
}

TEST_F(PreludeTest, ExcludeListFromNil)
{
    ScamValue expr = parseAndEvaluate("(exclude  (list 1 2 3) ())");
    expectNull(expr);
}

TEST_F(PreludeTest, ExcludeAllFromList)
{
    ScamValue expr =
        parseAndEvaluate("(define x (list 1 2 3)) (exclude x x)");
    expectNull(expr);
}

TEST_F(PreludeTest, ExcludePartial)
{
    ScamValue expr =
        parseAndEvaluate("(define x (list 1 2 3)) (exclude (cdr x) x)");
    expectList(expr, "(1)", 1);
}

TEST_F(PreludeTest, ExcludeNonOverlapping)
{
    ScamValue expr = parseAndEvaluate("(exclude (list 1 2) (list 3 4))");
    expectList(expr, "(3 4)", 2);
}

TEST_F(PreludeTest, LengthVector)
{
    ScamValue expr = parseAndEvaluate("(old-length #(1 2 3))");
    expectInteger(expr, 3, "3", true);
}

TEST_F(PreludeTest, LengthDict)
{
    ScamValue expr = parseAndEvaluate("(old-length { :a 44 :b \"cat\" })");
    expectInteger(expr, 2, "2", true);
}

TEST_F(PreludeTest, LengthBadType)
{
    ScamValue expr = parseAndEvaluate("(old-length :abc)");
    expectError(expr);
}

TEST_F(PreludeTest, NthList)
{
    ScamValue expr = parseAndEvaluate("(nth 0 '(a b c))");
    expectSymbol(expr, "a");
}

TEST_F(PreludeTest, NthListOutOfBounds)
{
    ScamValue expr = parseAndEvaluate("(nth 4 '(a b c))");
    expectError(expr);
}

TEST_F(PreludeTest, NthVector)
{
    ScamValue expr = parseAndEvaluate("(nth 0 #(1 2 3))");
    expectInteger(expr, 1, "1", true);
}

TEST_F(PreludeTest, NthVectorOutOfBounds)
{
    ScamValue expr = parseAndEvaluate("(nth -50 #(1 2 3))");
    expectError(expr);
}

TEST_F(PreludeTest, NthNotIndexible)
{
    ScamValue expr = parseAndEvaluate("(nth 0 { :a 333.333 })");
    expectError(expr);
}

TEST_F(PreludeTest, CrossNilWithNil)
{
    ScamValue expr = parseAndEvaluate("(cross cons '() '())");
    expectNull(expr);
}

TEST_F(PreludeTest, CrossNilWithList)
{
    ScamValue expr = parseAndEvaluate("(cross cons '() '((1) (2)))");
    expectNull(expr);
}

TEST_F(PreludeTest, CrossListWithNil)
{
    ScamValue expr =
        parseAndEvaluate("(cross (lambda (a b) (+ a b)) '(1 2 3) '())");
    expectNull(expr);
}

TEST_F(PreludeTest, CrossListWithList)
{
    ScamValue expr =
        parseAndEvaluateFile("scripts/prelude/crosstest1.scm");
    expectList(expr, "(2 3 4 3 4 5 4 5 6)", 9);
}

TEST_F(PreludeTest, PowerSetNil)
{
    ScamValue expr = parseAndEvaluate("(power-set '())");
    expectList(expr, "(())", 1);
}

TEST_F(PreludeTest, PowerSetSingleton)
{
    ScamValue expr = parseAndEvaluate("(power-set '(1))");
    expectList(expr, "(() (1))", 2);
}

TEST_F(PreludeTest, PowerSetList)
{
    ScamValue expr = parseAndEvaluateFile("scripts/prelude/powerset.scm");
    expectList(expr, "(#t #t #t #t)", 4);
}


TEST_F(PreludeTest, SomeOfNone)
{
    ScamValue expr = parseAndEvaluate("(some-of '())");
    expectError(expr, "No more choices", false);
}

TEST_F(PreludeTest, SomeOfOne)
{
    ScamValue expr = parseAndEvaluate("(some-of '(1))");
    expectList(expr, "(1)", 1);

    expr = parseAndEvaluate("?");
    expectError(expr, "No more choices", false);
}

TEST_F(PreludeTest, CondNoClauses)
{
    ScamValue expr = parseAndEvaluate("(cond ())");
    expectNull(expr);
}

TEST_F(PreludeTest, CondOneTrueClause)
{
    ScamValue expr = parseAndEvaluateFile("scripts/cond/onetrue.scm");
    expectString(expr, "\"One\"");
}

TEST_F(PreludeTest, CondOneFalseClause)
{
    ScamValue expr = parseAndEvaluateFile("scripts/cond/onefalse.scm");
    expectNull(expr);
}

TEST_F(PreludeTest, CondManyClauses)
{
    ScamValue expr = parseAndEvaluateFile("scripts/cond/many.scm");
    expectString(expr, "\"Last\"");
}

TEST_F(PreludeTest, CondElse)
{
    ScamValue expr = parseAndEvaluateFile("scripts/cond/else.scm");
    expectString(expr, "\"Else Evaluated\"");
}

TEST_F(PreludeTest, CondManyForms)
{
    ScamValue expr = parseAndEvaluate("(cond ((#t 1 2 3)))");
    expectInteger(expr, 3, "3", true);
}

TEST_F(PreludeTest, CondNoForms)
{
    ScamValue expr = parseAndEvaluate("(cond ((3)))");
    expectInteger(expr, 3, "3", true);
}

TEST_F(PreludeTest, CondArrowForm)
{
    (void) parseAndEvaluate("(define inc (lambda (x) (+ x 1)))");
    ScamValue expr = parseAndEvaluate("(cond ((1 => inc)))");
    expectInteger(expr, 2, "2", true);
}
