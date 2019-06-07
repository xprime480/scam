#include "TestBase.hpp"

#include "util/ArgListHelper.hpp"
#include "expr/TypePredicates.hpp"

#include "util/GlobalId.hpp"
#include "util/DebugTrace.hpp"
#include "expr/ValueWriter.hpp"

#include "util/GlobalId.hpp"
#include "util/DebugTrace.hpp"
#include "expr/ValueWriter.hpp"

using namespace std;
using namespace scam;

class ArgListHelperTest : public TestBase
{
protected:
    ArgListHelperTest()
        : TestBase(false)
        , context("ArgHelperTest")
    {
    }

    const string context;
};

TEST_F(ArgListHelperTest, PeekAtEmptyList)
{
    ScamValue args = readString("()");

    ArgListHelper helper(args);
    ScamValue next = helper.peek();
    expectNull(next);
}

TEST_F(ArgListHelperTest, PeekAtNonEmptyList)
{
    ScamValue args = readString("(2 3 4)");

    ArgListHelper helper(args);
    ScamValue next = helper.peek();
    expectInteger(next, 2, "2", true);

    ScamValue next2 = helper.peek();
    expectInteger(next2, 2, "2", true);
}

TEST_F(ArgListHelperTest, EmptyListOK)
{
    ScamValue args = readString("()");

    ArgListHelper helper(args);
    ScamValue status = helper.getStatus();
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, EmptyListWantOne)
{
    ScamValue args = readString("()");

    ArgListHelper helper(args);

    ScamValue value;
    ScamValue status = helper.getAnyValue(value);

    expectNull(value);
    expectError(status, "parameter 1 missing");
}

TEST_F(ArgListHelperTest, SingletonListWantOne)
{
    ScamValue args = readString("(:arg1)");
    ArgListHelper helper(args);

    ScamValue value;
    ScamValue status = helper.getAnyValue(value);
    expectNull(status);
    expectKeyword(value, ":arg1");

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, SingletonListWantTwo)
{
    ScamValue args = readString("(:arg1)");
    ArgListHelper helper(args);

    ScamValue arg1;
    ScamValue status = helper.getAnyValue(arg1);
    expectKeyword(arg1, ":arg1");
    expectNull(status);

    ScamValue arg2;
    status = helper.getAnyValue(arg2);
    expectNull(arg2);
    expectError(status, "parameter 2 missing");
}

TEST_F(ArgListHelperTest, HaveTwoWantOne)
{
    ScamValue args = readString("(:arg1 :arg2)");
    ArgListHelper helper(args);

    ScamValue status;

    ScamValue arg1;
    status = helper.getAnyValue(arg1);
    expectKeyword(arg1, ":arg1");
    expectNull(status);

    status = helper.finish();
    expectError(status, "1 parameter needed, 2 given");
}

TEST_F(ArgListHelperTest, GetOneCharacter)
{
    ScamValue args = readString("(#\\x)");

    ArgListHelper helper(args);
    ScamValue status;

    char arg1;
    status = helper.getCharacter(arg1);
    EXPECT_EQ('x', arg1);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetInteger)
{
    ScamValue args = readString("(-5)");

    ArgListHelper helper(args);
    ScamValue status;

    int arg1;
    status = helper.getInteger(arg1);
    EXPECT_EQ(-5, arg1);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetIntegerFailure)
{
    ScamValue args = readString("(:k)");

    ArgListHelper helper(args);
    ScamValue status;

    int arg1 { -2 };
    status = helper.getInteger(arg1);
    EXPECT_EQ(-2, arg1);
    expectError(status, "expected integer for parameter 1 got ':k'");
}

TEST_F(ArgListHelperTest, GetNonNegativeInteger)
{
    ScamValue args = readString("(32)");

    ArgListHelper helper(args);
    ScamValue status;

    int arg1;
    status = helper.getNonNegativeInteger(arg1);
    EXPECT_EQ(32, arg1);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetNonNegativeIntegerFailure)
{
    ScamValue args = readString("(-1)");

    ArgListHelper helper(args);
    ScamValue status;

    int arg1 { -2 };
    status = helper.getNonNegativeInteger(arg1);
    EXPECT_EQ(-2, arg1);
    expectError(status,
                "expected non-negative integer for parameter 1 got '-1'");
}

TEST_F(ArgListHelperTest, GetString)
{
    ScamValue args = readString("(\"help me\")");

    ArgListHelper helper(args);
    ScamValue status;

    string arg1;
    status = helper.getString(arg1);
    EXPECT_EQ("help me", arg1);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetStringFailure)
{
    ScamValue args = readString("(-1)");

    ArgListHelper helper(args);
    ScamValue status;

    string arg1;
    status = helper.getString(arg1);
    EXPECT_EQ("", arg1);
    expectError(status, "expected string for parameter 1 got '-1'");
}

TEST_F(ArgListHelperTest, GetIndexInRange)
{
    ScamValue args = readString("(\"help me\" 3)");

    ArgListHelper helper(args);
    ScamValue status;

    string arg1;
    status = helper.getString(arg1);
    EXPECT_EQ("help me", arg1);
    expectNull(status);

    int index { 0 };
    status = helper.getIndex(index, 0);
    EXPECT_EQ(3, index);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetIndexOutsideRange)
{
    ScamValue args = readString("(\"help me\" 55)");

    ArgListHelper helper(args);
    ScamValue status;

    string arg1;
    status = helper.getString(arg1);
    EXPECT_EQ("help me", arg1);
    expectNull(status);

    int index { 0 };
    status = helper.getIndex(index, 0);
    EXPECT_EQ(0, index);

    const char * err
    { "expected index into parameter 1 ('\"help me\"') "
            "for parameter 2 got '55'" };
    expectError(status, err);
}

TEST_F(ArgListHelperTest, GetCharacterListEmpty)
{
    ScamValue args = readString("()");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1;
    status = helper.getZeroPlus(arg1, isChar);
    expectNil(arg1);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetCharacterListNonEmpty)
{
    const char * text { "(#\\a #\\z)" };
    ScamValue args = readString(text);

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1;
    status = helper.getZeroPlus(arg1, isChar);
    expectList(arg1, text, 2);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetCountWant3to5Have2)
{
    const char * text { "(1 2)" };
    ScamValue args = readString(text);

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNull();
    status = helper.getCount(arg1, isInteger, 3, 5);
    expectNull(arg1);
    expectError(status, "found 2 values, wanted at least 3");
}

TEST_F(ArgListHelperTest, GetCountWant3to5Have4)
{
    const char * text { "(1 2 3 4)" };
    ScamValue args = readString(text);

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNull();
    status = helper.getCount(arg1, isInteger, 3, 5);
    expectList(arg1, text, 4);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetCountWant3to5Have6)
{
    const char * text { "(1 2 3 4 5 6)" };
    ScamValue args = readString(text);

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNull();
    status = helper.getCount(arg1, isInteger, 3, 5);
    expectList(arg1, "(1 2 3 4 5)", 5);
    expectNull(status);

    status = helper.finish();
    expectError(status, "5 parameter needed, 6 given");
}

TEST_F(ArgListHelperTest, GetOptionalCharacterMissing)
{
    ScamValue args = readString("()");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1;
    status = helper.getOptional(arg1, isChar);
    expectNull(arg1);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetOptionalCharacterPresent)
{
    ScamValue args = readString("(#\\$)");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1;
    status = helper.getOptional(arg1, isChar);
    expectChar(arg1, '$', "#\\$");
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetSublistEmpty)
{
    ScamValue args = readString("(())");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNull();
    status = helper.getSublistOf(arg1, isInteger);
    expectNil(arg1);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetSublistNonEmpty)
{
    ScamValue args = readString("((1 3 5 7))");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNull();
    status = helper.getSublistOf(arg1, isInteger);
    expectList(arg1, "(1 3 5 7)", 4);
    expectNull(status);

    status = helper.finish();
    expectNull(status);
}

TEST_F(ArgListHelperTest, GetSublistWrongType)
{
    ScamValue args = readString("((1 3 \"\" 7))");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNull();
    status = helper.getSublistOf(arg1, isInteger);
    expectNull(arg1);
    expectError(status, "list for parameter 1 contains invalid value '\"\"'");
}
