#include "TestBase.hpp"

using namespace std;
using namespace scam;

class ComparisonTest : public TestBase
{
protected:
    ComparisonTest()
        : TestBase(false)
    {
    }
};

TEST_F(ComparisonTest, CmpZeroForms)
{
    expectTrue("(=)");
    expectTrue("(<>)");
    expectTrue("(<)");
    expectTrue("(<=)");
    expectTrue("(>)");
    expectTrue("(>=)");
}

TEST_F(ComparisonTest, EqNumber)
{
    expectTrue("(= 3)");
    expectTrue("(= 3 3)");
    expectFalse("(= 3 6)");
    expectTrue("(= 3 3 3 3 3 3)");
    expectFalse("(= 3 3 3 3 3 5)");
    expectTrue("(= 2+3i 2+3i 2+3i)");
}

TEST_F(ComparisonTest, EqSpecialNumber)
{
    expectTrue("(= +inf.0)");
    expectTrue("(= -inf.0)");
    expectFalse("(= +nan.0)");

    expectTrue("(= +inf.0 +inf.0)");
    expectTrue("(= -inf.0 -inf.0)");
    expectFalse("(= +nan.0 +nan.0)");
}

TEST_F(ComparisonTest, EqString)
{
    expectTrue("(= \"A\")");
    expectTrue("(= \"A\" \"A\")");
    expectFalse("(= \"A\" \"Z\")");
    expectTrue("(= \"A\" \"A\" \"A\" \"A\" \"A\" \"A\")");
    expectFalse("(= \"A\" \"A\" \"A\" \"A\" \"A\" \"Z\")");
}

TEST_F(ComparisonTest, EqBadArgs)
{
    ScamValue expr = parseAndEvaluate("(= #t)");
    expectError(expr);

    expr = parseAndEvaluate("(= 3 \"x\")");
    expectError(expr);
}

TEST_F(ComparisonTest, NeNumber)
{
    expectTrue("(<> 3)");
    expectFalse("(<> 3 3)");
    expectTrue("(<> 3 6)");
    expectTrue("(<> 3 4 5 3)");  // pairwise check
    expectFalse("(<> 3 3 5)");
    expectTrue("(<> 7-5i 1.111+0.00001i)");
}

TEST_F(ComparisonTest, NeSpecialNumber)
{
    expectTrue("(<> -inf.0)");
    expectTrue("(<> +inf.0)");
    expectFalse("(<> +nan.0)");

    expectTrue("(<> -inf.0 +inf.0 -inf.0)");
    expectTrue("(<> +inf.0 -inf.0)");
    expectFalse("(<> +nan.0 +nan.0)");
}

TEST_F(ComparisonTest, NeString)
{
    expectTrue("(<> \"A\")");
    expectFalse("(<> \"A\" \"A\")");
    expectTrue("(<> \"A\" \"Z\")");
    expectTrue("(<> \"A\" \"Z\" \"A\")");
    expectFalse("(<> \"A\" \"A\" \"Z\")");
}

TEST_F(ComparisonTest, GtNumber)
{
    expectTrue("(> 3)");
    expectFalse("(> 3 3)");
    expectTrue("(> 3 2)");
    expectTrue("(> 4 3 2 1)");
    expectFalse("(> 4 3 2 5)");
    expectFalse("(> 2+i 1+i)");
    expectFalse("(> 1+i 2+i)");
}

TEST_F(ComparisonTest, GtSpecialNumber)
{
    expectTrue("(> -inf.0)");
    expectTrue("(> +inf.0)");
    expectFalse("(> +nan.0)");

    expectTrue("(> +inf.0 0 -inf.0)");
    expectFalse("(> -inf.0 -inf.0)");
    expectFalse("(> +nan.0 +nan.0)");
}

TEST_F(ComparisonTest, GtString)
{
    expectTrue("(> \"a\")");
    expectFalse("(> \"a\" \"a\")");
    expectTrue("(> \"b\" \"a\")");
    expectTrue("(> \"zz\" \"z\" \"w\" \"aaaazzz\")");
    expectFalse("(> \"z\" \"xa\" \"ez\" \"q\")");
}

TEST_F(ComparisonTest, GeNumber)
{
    expectTrue("(>= 3)");
    expectTrue("(>= 3 3)");
    expectFalse("(>= 2 3)");
    expectTrue("(>= 51 3 3 -4)");
    expectFalse("(>= 1 1 1 10)");
    expectFalse("(>= 2+i 1+i)");
    expectFalse("(>= 1+i 2+i)");
    expectTrue("(>= 1+i 1+i)");
}

TEST_F(ComparisonTest, GeSpecialNumber)
{
    expectTrue("(>= -inf.0)");
    expectTrue("(>= +inf.0)");
    expectFalse("(>= +nan.0)");

    expectTrue("(>= +inf.0 +inf.0 0  -inf.0  -inf.0)");
    expectFalse("(>= +nan.0 +nan.0)");
}

TEST_F(ComparisonTest, GeString)
{
    expectTrue("(>= \"a\")");
    expectTrue("(>= \"a\" \"a\")");
    expectTrue("(>= \"b\" \"a\")");
    expectTrue("(>= \"za\" \"fa\" \"ez\" \"\")");
    expectFalse("(>= \"zzz\" \"a\" \"z\" \"q\")");
}

TEST_F(ComparisonTest, LtNumber)
{
    expectTrue("(< 3)");
    expectFalse("(< 3 3)");
    expectTrue("(< 2 3)");
    expectTrue("(< 1 2 3 4)");
    expectFalse("(< 5 2 3 4)");
    expectFalse("(< 2+i 1+i)");
    expectFalse("(< 1+i 2+i)");
}

TEST_F(ComparisonTest, LtSpecialNumber)
{
    expectTrue("(< -inf.0)");
    expectTrue("(< +inf.0)");
    expectFalse("(< +nan.0)");

    expectTrue("(< -inf.0 0 +inf.0)");
    expectFalse("(< -inf.0 -inf.0)");
    expectFalse("(< +nan.0 +nan.0)");
}

TEST_F(ComparisonTest, LeNumber)
{
    expectTrue("(<= 3)");
    expectTrue("(<= 3 3)");
    expectFalse("(<= 3 2)");
    expectTrue("(<= -4 3 3 51)");
    expectFalse("(<= 10 1 1)");
    expectFalse("(<= 2+i 1+i)");
    expectFalse("(<= 1+i 2+i)");
    expectTrue("(<= 1+i 1+i)");
}

TEST_F(ComparisonTest, LeSpecialNumber)
{
    expectTrue("(<= -inf.0)");
    expectTrue("(<= +inf.0)");
    expectFalse("(<= +nan.0)");

    expectTrue("(<= -inf.0 -inf.0 0  +inf.0  +inf.0)");
    expectFalse("(<= +nan.0 +nan.0)");
}

