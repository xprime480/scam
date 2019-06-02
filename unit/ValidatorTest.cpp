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

    auto callback = [] (const ValidatorResult & result) -> void
    {
    };

    validate(context, args, cont, callback);

    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantStringEmptyList)
{
    ScamValue args = makeNil();
    auto callback = [] (const ValidatorResult & result) -> void
    {
        FAIL() << "callback should not be invoked";
    };

    validate(context, args, cont, callback, matchInteger(arg1));

    expectError(cont->getExpr(), "ValidatorTest: parameter list too short");
}

TEST_F(ValidatorTest, WantStringHaveString)
{
    const char * text { "\"roses are red\"" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        expectString(result.get(arg1), text);
    };

    validate(context, args, cont, callback, matchString(arg1));

    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantStringHaveNumber)
{
    const char * text { "2" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        FAIL() << "callback should not be invoked";
    };

    validate(context, args, cont, callback, matchString(arg1));

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
    auto callback = [] (const ValidatorResult & result) -> void
    {
        FAIL() << "callback should not be invoked";
    };

    validate(context, args, cont, callback, matchString(arg1));

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
        EXPECT_TRUE(result);
        expectInteger(result.get(arg1), 1960, text, true);
    };

    validate(context, args, cont, callback, matchInteger(arg1));

    expectNull(cont->getExpr());
}

TEST_F(ValidatorTest, WantIntegerHaveString)
{
    const char * text { "\"string\"" };
    ScamValue first = readString(text);
    ScamValue args = makeList(first);
    auto callback = [=] (const ValidatorResult & result) -> void
    {
        FAIL() << "callback should not be invoked";
    };

    validate(context, args, cont, callback, matchInteger(arg1));

    static const char * err =
        "ValidatorTest: expected integer for parm 'arg1', got '\"string\"'";
    expectError(cont->getExpr(), err);
}
