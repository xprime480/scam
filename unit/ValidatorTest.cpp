#include "TestBase.hpp"

#include "Extractor.hpp"
#include "expr/ValueFactory.hpp"
#include "util/Validator.hpp"
#include "util/MemoryManager.hpp"

using namespace std;
using namespace scam;

class ValidatorTest : public TestBase
{
protected:
    ValidatorTest()
        : context("ValidatorTest")
        , arg1("arg1")
        , cont(standardMemoryManager.make<Extractor>())
    {
    }

    const string context;
    const string arg1;
    Extractor * cont;
};

TEST_F(ValidatorTest, EmptyListOK)
{
    ScamValue args = makeNil();
    ValidatorResult result = validate(context, args, cont);

    EXPECT_TRUE(result);
    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantStringEmptyList)
{
    ScamValue args = makeNil();
    ValidatorResult result = validate(context, args, cont, matchString(arg1));

    EXPECT_FALSE(result);
    expectError(cont->getExpr(), "ValidatorTest: parameter list too short");
}

TEST_F(ValidatorTest, WantStringHaveString)
{
    const char * text { "\"roses are red\"" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    ValidatorResult result = validate(context, args, cont, matchString(arg1));

    EXPECT_TRUE(result);
    expectNull(cont->getExpr());
    expectString(result.get(arg1), text);
}

TEST_F(ValidatorTest, WantStringHaveNumber)
{
    const char * text { "2" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    ValidatorResult result = validate(context, args, cont, matchString(arg1));

    static const char * err =
        "ValidatorTest: expected string for parm 'arg1', got '2'";
    EXPECT_FALSE(result);
    expectError(cont->getExpr(), err);
    expectNull(result.get(arg1));
}

TEST_F(ValidatorTest, WantStringHaveTooManyArgs)
{
    const char * text { "\"first one\"" };
    const char * extra { "\"never mind\"" };
    ScamValue first = readString(text);
    ScamValue second = readString(extra);
    ScamValue args = makeList(first, second);
    ValidatorResult result = validate(context, args, cont, matchString(arg1));

    static const char * err =
        "ValidatorTest: excess arguments found: '(\"never mind\")'";
    EXPECT_FALSE(result);
    expectError(cont->getExpr(), err);
    expectString(result.get(arg1), text);
}

TEST_F(ValidatorTest, WantIntegerHaveInteger)
{
    const char * text { "1960" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    ValidatorResult result = validate(context, args, cont, matchInteger(arg1));

    EXPECT_TRUE(result);
    expectNull(cont->getExpr());
    expectInteger(result.get(arg1), 1960, text, true);
}

TEST_F(ValidatorTest, WantIntegerHaveString)
{
    const char * text { "\"string\"" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    ValidatorResult result = validate(context, args, cont, matchInteger(arg1));

    static const char * err =
        "ValidatorTest: expected integer for parm 'arg1', got '\"string\"'";
    EXPECT_FALSE(result);
    expectError(cont->getExpr(), err);
    expectNull(result.get(arg1));
}
