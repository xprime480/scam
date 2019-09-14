#include "TestBase.hpp"

#include "ScamException.hpp"
#include "expr/SequenceOps.hpp"
#include "value/ValueFactory.hpp"

using namespace std;
using namespace scam;

class SequenceOpsTest : public TestBase
{
protected:
    SequenceOpsTest()
    {
    }
};

TEST_F(SequenceOpsTest, CarEmpty)
{
    ScamValue value = makeNull();
    EXPECT_THROW(getCar(value), ScamException);
}

TEST_F(SequenceOpsTest, CarNonList)
{
    ScamValue value = readString("{}");
    EXPECT_THROW(getCar(value), ScamException);
}

TEST_F(SequenceOpsTest, CarNonEmpty)
{
    ScamValue value = readString("(1 2 3)");
    ScamValue expr = getCar(value);
    expectInteger(expr, 1, "1", true);
}

TEST_F(SequenceOpsTest, CdrEmpty)
{
    ScamValue value = makeNull();
    EXPECT_THROW(getCdr(value), ScamException);
}

TEST_F(SequenceOpsTest, CdrNonList)
{
    ScamValue value = readString("{}");
    EXPECT_THROW(getCdr(value), ScamException);
}

TEST_F(SequenceOpsTest, CdrNonEmpty)
{
    ScamValue value = readString("(1 2 3)");
    ScamValue expr = getCdr(value);
    expectList(expr, "(2 3)", 2);
}

TEST_F(SequenceOpsTest, LengthNil)
{
    ScamValue value = readString("()");
    ScamValue expr = makeInteger(length(value), true);
    expectInteger(expr, 0, "0", true);
}

TEST_F(SequenceOpsTest, LengthList)
{
    ScamValue value = readString("(a b c)");
    ScamValue expr = makeInteger(length(value), true);
    expectInteger(expr, 3, "3", true);
}

TEST_F(SequenceOpsTest, LengthVector)
{
    ScamValue value = readString("#(a b c)");
    ScamValue expr = makeInteger(length(value), true);
    expectInteger(expr, 3, "3", true);
}

TEST_F(SequenceOpsTest, LengthByteVector)
{
    ScamValue value = readString("#u8(23 42 17)");
    ScamValue expr = makeInteger(length(value), true);
    expectInteger(expr, 3, "3", true);
}

TEST_F(SequenceOpsTest, LengthDictionary)
{
    ScamValue value = readString("{ :key \"value\" }");
    ScamValue expr = makeInteger(length(value), true);
    expectInteger(expr, 1, "1", true);
}

TEST_F(SequenceOpsTest, LengthNonSequence)
{
    ScamValue value = readString("symbols-have-no-length");
    EXPECT_THROW(length(value), ScamException);
}

TEST_F(SequenceOpsTest, NthCarList)
{
    ScamValue value = readString("(four score and seven)");
    ScamValue expr = nthcar(value, 3);
    expectSymbol(expr, "seven");
}

TEST_F(SequenceOpsTest, NthCarByteVector)
{
    ScamValue value = readString("(4 20 0 7)");
    ScamValue expr = nthcar(value, 3);
    expectInteger(expr, 7, "7", true);
}

TEST_F(SequenceOpsTest, NthCarVector)
{
    ScamValue value = readString("#(four score and seven)");
    ScamValue expr = nthcar(value, 3);
    expectSymbol(expr, "seven");
}

TEST_F(SequenceOpsTest, NthCarListOutOfRange)
{
    ScamValue value = readString("(four score and seven)");
    expectError(nthcar(value, 666));
}

TEST_F(SequenceOpsTest, NthCarNonSequence)
{
    ScamValue value = readString("2");
    EXPECT_THROW(nthcar(value, 0), ScamException);
}

TEST_F(SequenceOpsTest, NthCdrList)
{
    ScamValue value = readString("(four score and seven)");
    ScamValue expr = nthcdr(value, 1);
    expectList(expr, "(and seven)", 2);
}

TEST_F(SequenceOpsTest, NthCdrListOutOfRange)
{
    ScamValue value = readString("(four score and seven)");
    expectError(nthcdr(value, 666));
}

TEST_F(SequenceOpsTest, NthCdrNonSequence)
{
    ScamValue value = readString("2");
    EXPECT_THROW(nthcar(value, 0), ScamException);
}

TEST_F(SequenceOpsTest, AppendNormalCase)
{
    ScamValue base = readString("(2)");
    ScamValue tail = readString("3");
    ScamValue expr = append(base, tail);
    expectList(expr, "(2 3)", 2);
}

TEST_F(SequenceOpsTest, AppendNilBase)
{
    ScamValue base = readString("()");
    ScamValue tail = readString("3");
    ScamValue expr = append(base, tail);
    expectList(expr, "(3)", 1);
}

TEST_F(SequenceOpsTest, AppendNilTail)
{
    ScamValue base = readString("(1 2 3)");
    ScamValue tail = readString("()");
    ScamValue expr = append(base, tail);
    expectList(expr, "(1 2 3 ())", 4);
}


TEST_F(SequenceOpsTest, AppendBaseNotList)
{
    ScamValue base = readString("2");
    ScamValue tail = readString("()");
    EXPECT_THROW(append(base, tail), ScamException);
}
