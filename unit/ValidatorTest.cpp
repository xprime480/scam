#include "TestBase.hpp"

#include "util/Validator.hpp"

#include "util/GlobalId.hpp"
#include "util/DebugTrace.hpp"
#include "expr/ValueWriter.hpp"

using namespace std;
using namespace scam;

class ValidatorTest : public TestBase
{
protected:
    ValidatorTest()
        : TestBase(false)
    {
    }
};

TEST_F(ValidatorTest, NothingForObject)
{
    ObjectValidator validator;

    EXPECT_FALSE(validator.isComplete());
    expectNothing(validator.get());

    ScamValue value = makeNothing();
    EXPECT_FALSE(validator.accept(value));
    EXPECT_FALSE(validator.isComplete());
    expectNothing(validator.get());
}

TEST_F(ValidatorTest, NullForObject)
{
    ObjectValidator validator;

    ScamValue value = makeNull();
    EXPECT_TRUE(validator.accept(value));
    EXPECT_TRUE(validator.isComplete());
    expectNull(validator.get());
}

TEST_F(ValidatorTest, StringForObject)
{
    ObjectValidator validator;
    ScamValue value = makeString("abc");
    EXPECT_TRUE(validator.accept(value));
    EXPECT_TRUE(validator.isComplete());
    expectString(validator.get(), "\"abc\"");
}

TEST_F(ValidatorTest, NullForNumeric)
{
    GlobalId id;
    ScamTraceScope _;
    scamTrace(id, __FILE__, __LINE__, __FUNCTION__);

    NumericValidator validator;

    ScamValue value = makeNull();
    EXPECT_FALSE(validator.accept(value));
    EXPECT_FALSE(validator.isComplete());
    expectNothing(validator.get());
}

TEST_F(ValidatorTest, NaNForNumeric)
{
    NumericValidator validator;

    ScamValue value = makeNaN();
    EXPECT_TRUE(validator.accept(value));
    EXPECT_TRUE(validator.isComplete());
    expectSpecialNumeric(validator.get(), "+nan.0");
}

TEST_F(ValidatorTest, IntegerForNumeric)
{
    NumericValidator validator;

    ScamValue value = makeInteger(1, true);
    EXPECT_TRUE(validator.accept(value));
    EXPECT_TRUE(validator.isComplete());
    expectInteger(validator.get(), 1, "1", true);
}

TEST_F(ValidatorTest, StringForString)
{
    StringValidator validator;

    ScamValue value = makeString("foo");
    EXPECT_TRUE(validator.accept(value));
    EXPECT_TRUE(validator.isComplete());
    expectString(validator.get(), "\"foo\"");
}

TEST_F(ValidatorTest, IntegerForString)
{
    StringValidator validator;

    ScamValue value = makeInteger(1, true);
    EXPECT_FALSE(validator.accept(value));
    EXPECT_FALSE(validator.isComplete());
    expectNothing(validator.get());
}



/////////////// ************************************  ////////////////////

TEST_F(ValidatorTest, MatchEmptySuccess)
{
    ScamValue args = makeNull();
    ScamValue result = matchList(args);

    expectNothing(result);
}

TEST_F(ValidatorTest, MatchEmptyArgsTooLong)
{
    ScamValue args = makeList(makeInteger(2, true));
    ScamValue result = matchList(args);

    expectError(result);
}

TEST_F(ValidatorTest, MatchIntegerArgsTooShort)
{
    NumericValidator v1;
    ScamValue args = makeNull();
    ScamValue result = matchList(args, v1);

    expectError(result);
}

TEST_F(ValidatorTest, MatchIntegerWrongType)
{
    NumericValidator v1;
    ScamValue args = makeList(makeSymbol("groovy"));
    ScamValue result = matchList(args, v1);

    expectError(result);
}

TEST_F(ValidatorTest, MatchIntegerSuccess)
{
    NumericValidator v1;
    ScamValue args = makeList(makeInteger(2, true));
    ScamValue result = matchList(args, v1);

    expectNothing(result);
    expectInteger(v1.get(), 2, "2", true);
}
