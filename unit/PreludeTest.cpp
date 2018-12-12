
#include "ExpressionTestBase.hpp"

using namespace std;
using namespace scam;

class PreludeTest : public ExpressionTestBase
{
};

TEST_F(PreludeTest, MaxIntegerTest)
{
    ExprHandle expr = parseAndEvaluate("(max 123 -123)");
    expectInteger(expr, 123, "123");
}

TEST_F(PreludeTest, MaxFloatTest)
{
    ExprHandle expr = parseAndEvaluate("(max 42.01 17.5)");
    expectFloat(expr, 42.01, "42.01");
}

TEST_F(PreludeTest, MaxMixedNumericTest)
{
    ExprHandle expr = parseAndEvaluate("(max -5 -9.999)");
    expectInteger(expr, -5, "-5");
}

TEST_F(PreludeTest, MinIntegerTest)
{
    ExprHandle expr = parseAndEvaluate("(min 123 -123)");
    expectInteger(expr, -123, "-123");
}

TEST_F(PreludeTest, MinFloatTest)
{
    ExprHandle expr = parseAndEvaluate("(min 42.01 17.5)");
    expectFloat(expr, 17.5, "17.5");
}

TEST_F(PreludeTest, MinMixedNumericTest)
{
    ExprHandle expr = parseAndEvaluate("(min -5 -9.999)");
    expectFloat(expr, -9.999, "-9.999");
}

TEST_F(PreludeTest, MapTest)
{
    ExprHandle expr = parseAndEvaluate("(map integer? (list 1 0.123 \"xx\"))");
    expectList(expr, "(#t #f #f)", 3);
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

    ExprHandle expr = parseAndEvaluate("(require #f)");
    expectError(expr, "No more choices");
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
    ExprHandle expr = parseAndEvaluate("(one-of (list))");
    expectError(expr, "No more choices");
}

TEST_F(PreludeTest, OneofOne)
{
    ExprHandle expr = parseAndEvaluate("(one-of (list 2))");
    expectInteger(expr, 2, "2");
}

TEST_F(PreludeTest, OneofSecondofThree)
{
    ExprHandle expr = parseAndEvaluate("(one-of (list 2 8 22)) ?");
    expectInteger(expr, 8, "8");
}

TEST_F(PreludeTest, ExcludeNilFromNil)
{
    ExprHandle expr = parseAndEvaluate("(exclude () ())");
    expectNil(expr);
}

TEST_F(PreludeTest, ExcludeNilFromList)
{
    ExprHandle expr = parseAndEvaluate("(exclude () (list 1 2 3))");
    expectList(expr, "(1 2 3)", 3);
}

TEST_F(PreludeTest, ExcludeListFromNil)
{
    ExprHandle expr = parseAndEvaluate("(exclude  (list 1 2 3) ())");
    expectNil(expr);
}

TEST_F(PreludeTest, ExcludeAllFromList)
{
    ExprHandle expr = parseAndEvaluate("(define x (list 1 2 3)) (exclude x x)");
    expectNil(expr);
}

TEST_F(PreludeTest, ExcludePartial)
{
    ExprHandle expr = parseAndEvaluate("(define x (list 1 2 3)) (exclude (cdr x) x)");
    expectList(expr, "(1)", 1);
}

TEST_F(PreludeTest, ExcludeNonOverlapping)
{
    ExprHandle expr = parseAndEvaluate("(exclude (list 1 2) (list 3 4))");
    expectList(expr, "(3 4)", 2);
}
