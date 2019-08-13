#include "TestBase.hpp"

#include "ErrorCategory.hpp"
#include "ScamException.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/ValueFactory.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

class EnvTest : public TestBase
{
protected:
    ScamValue key;
    ScamValue exp;

    EnvTest()
        : key(makeNothing())
        , exp(makeNothing())
    {
    }

    void SetUp() override
    {
        TestBase::SetUp();
        reset(false);
    }

    void reset(bool init)
    {
        engine.reset(init);
        engine.pushHandler(handler);

        key = makeSymbol("key");
        exp = makeInteger(1, true);

        expectNothing(engine.addBinding(key, exp));
    }
};

TEST_F(EnvTest, Fetch)
{
    ScamValue act = engine.getBinding(key);
    EXPECT_EQ(asInteger(exp), asInteger(act));
}

TEST_F(EnvTest, FetchTraversesFrames)

{
    for ( size_t i = 0 ; i < 5 ; ++i ) {
        engine.pushFrame();
    }

    ScamValue act = engine.getBinding(key);
    EXPECT_EQ(asInteger(exp), asInteger(act));
}

TEST_F(EnvTest, DuplicateKeys)
{
    ScamValue val2 = makeInteger(2, true);
    ScamValue expr = engine.addBinding(key, val2);
    expectError(expr);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());

    ScamValue act = engine.getBinding(key);
    EXPECT_EQ(asInteger(exp), asInteger(act));
}

TEST_F(EnvTest, ExtensionTest)
{
    engine.pushFrame();
    ScamValue exp2 = makeInteger(2, true);
    expectNothing(engine.addBinding(key, exp2));

    ScamValue act2 = engine.getBinding(key);
    EXPECT_EQ(asInteger(exp2), asInteger(act2));

    // original environment is unchanged
    //
    engine.popFrame();
    ScamValue act = engine.getBinding(key);
    EXPECT_EQ(asInteger(exp), asInteger(act));
}

TEST_F(EnvTest, Assign)
{
    ScamValue newExp = makeInteger(33, true);
    expectNothing(engine.rebind(key, newExp));
    ScamValue act = engine.getBinding(key);
    EXPECT_EQ(asInteger(newExp), asInteger(act));
}

TEST_F(EnvTest, AssignToNonexistentKey)
{
    ScamValue newKey = makeSymbol("*bad*");
    ScamValue newExp = makeInteger(33, true);
    ScamValue test = engine.rebind(newKey, newExp);
    ASSERT_TRUE(isError(test));
    EXPECT_EQ(envCategory, test->errorCategory());
}

TEST_F(EnvTest, AssignTraversesFrames)
{
    engine.pushFrame();
    ScamValue newExp = makeInteger(33, true);
    expectNothing(engine.rebind(key, newExp));
    ScamValue act = engine.getBinding(key);
    EXPECT_EQ(asInteger(newExp), asInteger(act));

    // it's in the original env
    //
    engine.popFrame();
    act = engine.getBinding(key);
    EXPECT_EQ(asInteger(newExp), asInteger(act));
}

TEST_F(EnvTest, Check)
{
    ScamValue key2 = makeSymbol("bad");

    expectBoolean(engine.hasBinding(key),  true,  "#t");
    expectBoolean(engine.hasBinding(key2), false, "#f");
}

TEST_F(EnvTest, CheckCurrentOnly)
{
    expectBoolean(engine.hasBinding(key, false), true, "#t");
    engine.pushFrame();
    expectBoolean(engine.hasBinding(key, false), false, "#f");
}

TEST_F(EnvTest, NullKey)
{
    ScamValue expr = engine.addBinding(nullptr, exp);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());

    expr = engine.hasBinding(nullptr);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());

    expr = engine.getBinding(nullptr);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());

    expr = engine.rebind(nullptr, exp);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());
}

TEST_F(EnvTest, GetTopLevel)
{
    reset(true);
    readEval("(define x 1)");
    engine.pushFrame();
    readEval("(define x 2)");

    ScamValue sym = makeSymbol("x");
    ScamValue val = engine.getBinding(sym, true);
    expectInteger(val, 1, "1", true);
}
