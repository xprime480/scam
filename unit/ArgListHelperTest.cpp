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
    expectNothing(next);
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
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, EmptyListWantOne)
{
    ScamValue args = readString("()");

    ArgListHelper helper(args);

    ScamValue value;
    ScamValue status = helper.getAnyValue(value);

    expectNothing(value);
    expectError(status, "parameter 1 missing");
}

TEST_F(ArgListHelperTest, SingletonListWantOne)
{
    ScamValue args = readString("(:arg1)");
    ArgListHelper helper(args);

    ScamValue value;
    ScamValue status = helper.getAnyValue(value);
    expectNothing(status);
    expectKeyword(value, ":arg1");

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, SingletonListWantTwo)
{
    ScamValue args = readString("(:arg1)");
    ArgListHelper helper(args);

    ScamValue arg1;
    ScamValue status = helper.getAnyValue(arg1);
    expectKeyword(arg1, ":arg1");
    expectNothing(status);

    ScamValue arg2;
    status = helper.getAnyValue(arg2);
    expectNothing(arg2);
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
    expectNothing(status);

    status = helper.finish();
    expectError(status, "Expected 1 values, got 2");
}

TEST_F(ArgListHelperTest, GetOneCharacter)
{
    ScamValue args = readString("(#\\x)");

    ArgListHelper helper(args);
    ScamValue status;

    char arg1;
    status = helper.getCharacter(arg1);
    EXPECT_EQ('x', arg1);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetInteger)
{
    ScamValue args = readString("(-5)");

    ArgListHelper helper(args);
    ScamValue status;

    int arg1;
    status = helper.getInteger(arg1);
    EXPECT_EQ(-5, arg1);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetIntegerFailure)
{
    ScamValue args = readString("(:k)");

    ArgListHelper helper(args);
    ScamValue status;

    int arg1 { -2 };
    status = helper.getInteger(arg1);
    EXPECT_EQ(-2, arg1);
    expectError(status, "wanted integer for parameter 1 (:k)");
}

TEST_F(ArgListHelperTest, GetNonNegativeInteger)
{
    ScamValue args = readString("(32)");

    ArgListHelper helper(args);
    ScamValue status;

    int arg1;
    status = helper.getNonNegativeInteger(arg1);
    EXPECT_EQ(32, arg1);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetNonNegativeIntegerFailure)
{
    ScamValue args = readString("(-1)");

    ArgListHelper helper(args);
    ScamValue status;

    int arg1 { -2 };
    status = helper.getNonNegativeInteger(arg1);
    EXPECT_EQ(-2, arg1);
    expectError(status, "wanted non-negative integer for parameter 1 (-1)");
}

TEST_F(ArgListHelperTest, GetString)
{
    ScamValue args = readString("(\"help me\")");

    ArgListHelper helper(args);
    ScamValue status;

    string arg1;
    status = helper.getString(arg1);
    EXPECT_EQ("help me", arg1);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetStringFailure)
{
    ScamValue args = readString("(-1)");

    ArgListHelper helper(args);
    ScamValue status;

    string arg1;
    status = helper.getString(arg1);
    EXPECT_EQ("", arg1);
    expectError(status, "wanted string for parameter 1 (-1)");
}

TEST_F(ArgListHelperTest, GetIndexInRange)
{
    ScamValue args = readString("(\"help me\" 3)");

    ArgListHelper helper(args);
    ScamValue status;

    string arg1;
    status = helper.getString(arg1);
    EXPECT_EQ("help me", arg1);
    expectNothing(status);

    int index { 0 };
    status = helper.getIndex(index, 0);
    EXPECT_EQ(3, index);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetIndexOutsideRange)
{
    ScamValue args = readString("(\"help me\" 55)");

    ArgListHelper helper(args);
    ScamValue status;

    string arg1;
    status = helper.getString(arg1);
    EXPECT_EQ("help me", arg1);
    expectNothing(status);

    int index { 0 };
    status = helper.getIndex(index, 0);
    EXPECT_EQ(0, index);

    const char * err
    { "wanted index into parameter 1 ('\"help me\"') for parameter 2 (55)" };
    expectError(status, err);
}

TEST_F(ArgListHelperTest, GetCharacterListEmpty)
{
    ScamValue args = readString("()");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1;
    status = helper.getZeroPlus(arg1, isChar);
    expectNull(arg1);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
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
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetCountWant3to5Have2)
{
    const char * text { "(1 2)" };
    ScamValue args = readString(text);

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNothing();
    status = helper.getCount(arg1, isInteger, 3, 5);
    expectNothing(arg1);
    expectError(status, "Too Few Values (2 of at least 3)");
}

TEST_F(ArgListHelperTest, GetCountWant3to5Have4)
{
    const char * text { "(1 2 3 4)" };
    ScamValue args = readString(text);

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNothing();
    status = helper.getCount(arg1, isInteger, 3, 5);
    expectList(arg1, text, 4);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetCountWant3to5Have6)
{
    const char * text { "(1 2 3 4 5 6)" };
    ScamValue args = readString(text);

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNothing();
    status = helper.getCount(arg1, isInteger, 3, 5);
    expectList(arg1, "(1 2 3 4 5)", 5);
    expectNothing(status);

    status = helper.finish();
    expectError(status, "Expected 5 values, got 6", true);
}

TEST_F(ArgListHelperTest, GetOptionalCharacterMissing)
{
    ScamValue args = readString("()");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1;
    status = helper.getOptional(arg1, isChar);
    expectNothing(arg1);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetOptionalCharacterPresent)
{
    ScamValue args = readString("(#\\$)");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1;
    status = helper.getOptional(arg1, isChar);
    expectChar(arg1, '$', "#\\$");
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetSublistEmpty)
{
    ScamValue args = readString("(())");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNothing();
    status = helper.getSublistOf(arg1, isInteger);
    expectNull(arg1);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetSublistNonEmpty)
{
    ScamValue args = readString("((1 3 5 7))");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNothing();
    status = helper.getSublistOf(arg1, isInteger);
    expectList(arg1, "(1 3 5 7)", 4);
    expectNothing(status);

    status = helper.finish();
    expectNothing(status);
}

TEST_F(ArgListHelperTest, GetSublistWrongType)
{
    ScamValue args = readString("((1 3 :keyword 7))");

    ArgListHelper helper(args);
    ScamValue status;

    ScamValue arg1 = makeNothing();
    status = helper.getSublistOf(arg1, isInteger);
    expectNothing(arg1);
    expectError(status, "invalid value at position 3 (:keyword)");
}

