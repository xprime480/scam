#include "TestBase.hpp"

#include "Extractor.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "util/MemoryManager.hpp"
#include "util/Validator.hpp"

using namespace std;
using namespace scam;

class ValidatorTest : public TestBase
{
protected:
    ValidatorTest()
        : context("ValidatorTest")
        , arg1("arg1")
        , arg2("arg2")
        , arg3("arg3")
        , arg4("arg4")
        , arg5("arg5")
        , callbackInvoked(false)
        , cont(standardMemoryManager.make<Extractor>())
    {
    }

    const string context;
    const string arg1;
    const string arg2;
    const string arg3;
    const string arg4;
    const string arg5;
    bool callbackInvoked;
    Extractor * cont;
};

TEST_F(ValidatorTest, EmptyListOK)
{
    ScamValue args = makeNil();

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
    };

    validate(context, args, cont, callback);

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantStringEmptyList)
{
    ScamValue args = makeNil();
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context, args, cont, callback, matchInteger(arg1));

    EXPECT_FALSE(callbackInvoked);
    expectError(cont->getExpr(), "ValidatorTest: no value for parameter arg1");
}

TEST_F(ValidatorTest, WantStringHaveString)
{
    const char * text { "\"roses are red\"" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        expectString(result.get(arg1), text);
    };

    validate(context, args, cont, callback, matchString(arg1));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantStringHaveNumber)
{
    const char * text { "2" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context, args, cont, callback, matchString(arg1));

    EXPECT_FALSE(callbackInvoked);
    static const char * err =
        "ValidatorTest: expected string for parm 'arg1', got '2'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, WantStringHaveTooManyArgs)
{
    const char * text { "\"first one\"" };
    const char * extra { "\"never mind\"" };
    ScamValue first = readString(text);
    ScamValue second = readString(extra);
    ScamValue args = makeList(first, second);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context, args, cont, callback, matchString(arg1));

    EXPECT_FALSE(callbackInvoked);
    static const char * err =
        "ValidatorTest: excess arguments found: '(\"never mind\")'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, WantIntegerHaveInteger)
{
    const char * text { "1960" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        EXPECT_TRUE(result);
        expectInteger(result.get(arg1), 1960, text, true);
    };

    validate(context, args, cont, callback, matchInteger(arg1));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantIntegerHaveString)
{
    const char * text { "\"string\"" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context, args, cont, callback, matchInteger(arg1));

    EXPECT_FALSE(callbackInvoked);
    static const char * err =
        "ValidatorTest: expected integer for parm 'arg1', got '\"string\"'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, WantNonNegativeIntegerHaveNegative)
{
    const char * text { "-1" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context, args, cont, callback, matchNonNegativeInteger(arg1));

    EXPECT_FALSE(callbackInvoked);
    static const char * err =
        "ValidatorTest: "
        "expected non-negative integer for parm 'arg1', got '-1'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, WantNonNegativeIntegerHaveZero)
{
    const char * text { "0" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        EXPECT_TRUE(result);
        expectInteger(result.get(arg1), 0, text, true);
    };

    validate(context, args, cont, callback, matchNonNegativeInteger(arg1));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantCharacter)
{
    const char * text { "#\\X" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        EXPECT_TRUE(result);
        expectChar(result.get(arg1), 'X', text);
    };

    validate(context, args, cont, callback, matchCharacter(arg1));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantSequenceHaveInsufficient)
{
    ScamValue args = readString("(13)");
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context,
             args,
             cont,
             callback,
             matchSequence(arg4,
                           matchNonNegativeInteger(arg1),
                           matchCharacter(arg2)));

    EXPECT_FALSE(callbackInvoked);
    expectError(cont->getExpr(), "ValidatorTest: no value for parameter arg2");
}

TEST_F(ValidatorTest, WantSequenceHaveSequence)
{
    const char * text1 { "13" };
    const char * text2 { "#\\X" };
    ScamValue parm1 = readString(text1);
    ScamValue parm2 = readString(text2);
    ScamValue args = makeList(parm1, parm2);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        EXPECT_TRUE(result);
        expectInteger(result.get(arg1), 13, text1, true);
        expectChar(result.get(arg2), 'X', text2);
    };

    validate(context,
             args,
             cont,
             callback,
             matchSequence(arg4,
                           matchNonNegativeInteger(arg1),
                           matchCharacter(arg2)));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantSequenceHaveExcess)
{
    const char * text1 { "13" };
    const char * text2 { "#\\X" };
    const char * text3 { "foo" };
    ScamValue parm1 = readString(text1);
    ScamValue parm2 = readString(text2);
    ScamValue parm3 = readString(text3);
    ScamValue args = makeList(parm1, parm2, parm3);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context,
             args,
             cont,
             callback,
             matchSequence(arg4,
                           matchNonNegativeInteger(arg1),
                           matchCharacter(arg2)));

    EXPECT_FALSE(callbackInvoked);
    static const char * err = "ValidatorTest: excess arguments found: '(foo)'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, WantSequenceLongish)
{
    const char * text1 { "13" };
    const char * text2 { "-13" };
    const char * text3 { "0" };
    const char * text4 { "480" };
    ScamValue args = readString("(13 -13 0 #i480)");

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        expectInteger(result.get(arg1), 13,  text1, true);
        expectInteger(result.get(arg2), -13, text2, true);
        expectInteger(result.get(arg3), 0,   text3, true);
        expectInteger(result.get(arg4), 480, text4, false);
    };

    validate(context,
             args,
             cont,
             callback,
             matchSequence(arg5,
                           matchNonNegativeInteger(arg1),
                           matchInteger(arg2),
                           matchInteger(arg3),
                           matchInteger(arg4)));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantAlternativeNoneMatch)
{
    const char * text1 { "13" };
    ScamValue parm1 = readString(text1);
    ScamValue args = makeList(parm1);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context,
             args,
             cont,
             callback,
             matchAlternative(arg4,
                              matchString(arg1),
                              matchCharacter(arg2)));

    EXPECT_FALSE(callbackInvoked);
    static const char * err =
        "ValidatorTest: no alternative was accepted for input '(13)'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, WantAlternativeFirstMatch)
{
    const char * text1 { "13" };
    ScamValue parm1 = readString(text1);
    ScamValue args = makeList(parm1);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        expectInteger(result.get(arg1), 13, text1, true);
        expectNull(result.get(arg2));
    };

    validate(context,
             args,
             cont,
             callback,
             matchAlternative(arg4,
                              matchInteger(arg1),
                              matchCharacter(arg2)));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantAlternativeSecondMatch)
{
    const char * text1 { "13" };
    ScamValue parm1 = readString(text1);
    ScamValue args = makeList(parm1);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        expectNull(result.get(arg1));
        expectInteger(result.get(arg2), 13, text1, true);
    };

    validate(context,
             args,
             cont,
             callback,
             matchAlternative(arg4,
                              matchCharacter(arg1),
                              matchInteger(arg2)));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantAlternativeWaitForIt)
{
    const char * text1 { "13" };
    ScamValue parm1 = readString(text1);
    ScamValue args = makeList(parm1);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        expectInteger(result.get(arg2), 13, text1, true);
    };

    validate(context,
             args,
             cont,
             callback,
             matchAlternative(arg5,
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchCharacter(arg1),
                              matchInteger(arg2)));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantSomeIntegersZeroFoundOK)
{
    ScamValue args = makeNil();

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        expectNil(result.get(arg1));
    };

    validate(context,
             args,
             cont,
             callback,
             matchCount(arg1, arg4, matchInteger(arg4), 0, 9999));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantSomeIntegersZeroFoundMinimumOne)
{
    ScamValue args = makeNil();

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context,
             args,
             cont,
             callback,
             matchCount(arg1, arg4, matchInteger(arg4), 1, 9999));

    EXPECT_FALSE(callbackInvoked);
    const string err =
        "ValidatorTest: "
        "0 values found, wanted at least 1 for key arg1 input '()'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, WantSomeIntegersManyFoundOK)
{
    const char * text { "(1 3 5 -99 2)" };
    ScamValue args = readString(text);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        ScamValue value = result.get(arg1);
        expectList(value, text, 5);
        expectInteger(getCar(value), 1, "1", true);
        expectInteger(nthcar(value, 3), -99, "-99", true);
    };

    validate(context,
             args,
             cont,
             callback,
             matchCount(arg1, arg4, matchInteger(arg4), 0, 9999));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantSimpleSublist)
{
    const char * text { "((a))" };
    ScamValue args = readString(text);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        ScamValue value = result.get(arg2);
        expectList(value, "(a)", 1);
        expectSymbol(getCar(value), "a");
    };

    validate(context,
             args,
             cont,
             callback,
             matchSublist(arg2, arg3, matchSymbol(arg3)));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantSomeNestedLists)
{
    const char * text { "((a 1) (b 2))" };
    ScamValue args = readString(text);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;

        ScamValue value = result.get(arg1);
        expectList(value, text, 2);

        ScamValue car = getCar(value);
        expectList(car, "(a 1)", 2);

        ScamValue caar = nthcar(car, 0);
        expectSymbol(caar, "a");

        ScamValue cadar = nthcar(car, 1);
        expectInteger(cadar, 1, "1", true);
    };

    validate(context,
             args,
             cont,
             callback,
             matchCount(arg1,
                        arg2,
                        matchSublist(arg2,
                                     arg5,
                                     matchSequence(arg5,
                                                   matchSymbol(arg3),
                                                   matchInteger(arg4))),
                        0,
                        9999));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, StringWithSingleIndexOutOfBounds)
{
    const char * text { "(\"\" 0)" };
    ScamValue args = readString(text);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context,
             args,
             cont,
             callback,
             matchSequence(arg5, matchString(arg3), matchIndex(arg4, arg3)));

    EXPECT_FALSE(callbackInvoked);
    const string err =
        "ValidatorTest: expected index for parm 'arg4', got '0'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, StringWithStartIndexOutOfBounds)
{
    const char * text { "(\"a string\" 600)" };
    ScamValue args = readString(text);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context,
             args,
             cont,
             callback,
             matchSequence(arg5,
                           matchString(arg3),
                           matchStartIndex(arg4, arg3)));

    EXPECT_FALSE(callbackInvoked);
    const string err =
        "ValidatorTest: expected start index for parm 'arg4', got '600'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, StringWithEndIndexOutOfBounds)
{
    const char * text { "(\"a string\" 0 600)" };
    ScamValue args = readString(text);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context,
             args,
             cont,
             callback,
             matchSequence(arg5,
                           matchString(arg3),
                           matchStartIndex(arg4, arg3),
                           matchEndIndex(arg1, arg3, arg4)));

    EXPECT_FALSE(callbackInvoked);
    const string err =
        "ValidatorTest: expected end index for parm 'arg1', got '600'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, StringWithEndValidIndicesOutOfOrder)
{
    const char * text { "(\"a string\" 2 0)" };
    ScamValue args = readString(text);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        FAIL() << "callback should not be invoked";
    };

    validate(context,
             args,
             cont,
             callback,
             matchSequence(arg5,
                           matchString(arg3),
                           matchStartIndex(arg4, arg3),
                           matchEndIndex(arg1, arg3, arg4)));

    EXPECT_FALSE(callbackInvoked);
    const string err =
        "ValidatorTest: expected end index for parm 'arg1', got '0'";
    expectError(cont->getExpr(), err);
}

TEST_F(ValidatorTest, StringWithIndicesOK)
{
    const char * text { "(\"a string\" 4 5)" };
    ScamValue args = readString(text);

    auto callback = [=] (const ValidatorResult & result) -> void
    {
        callbackInvoked = true;
        ScamValue str = result.get(arg3);
        expectString(str, "\"a string\"");
        ScamValue start = result.get(arg4);
        expectInteger(start, 4, "4", 4);
        ScamValue end = result.get(arg1);
        expectInteger(end, 5, "5", 4);
    };

    validate(context,
             args,
             cont,
             callback,
             matchSequence(arg5,
                           matchString(arg3),
                           matchStartIndex(arg4, arg3),
                           matchEndIndex(arg1, arg3, arg4)));

    EXPECT_TRUE(callbackInvoked);
    expectNull(cont->getExpr());
}
