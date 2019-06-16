#include "Env.hpp"

#include "TestBase.hpp"
#include "ScamException.hpp"
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

        engine.addBinding(key, exp);
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
    EXPECT_THROW(engine.addBinding(key, val2), ScamException);

    ScamValue act = engine.getBinding(key);
    EXPECT_EQ(asInteger(exp), asInteger(act));
}

TEST_F(EnvTest, ExtensionTest)
{
    engine.pushFrame();
    ScamValue exp2 = makeInteger(2, true);
    engine.addBinding(key, exp2);

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
    engine.rebind(key, newExp);
    ScamValue act = engine.getBinding(key);
    EXPECT_EQ(asInteger(newExp), asInteger(act));
}

TEST_F(EnvTest, AssignToNonexistentKey)
{
    ScamValue newKey = makeSymbol("*bad*");
    ScamValue newExp = makeInteger(33, true);
    EXPECT_THROW(engine.rebind(newKey, newExp), ScamException);
}

TEST_F(EnvTest, AssignTraversesFrames)
{
    engine.pushFrame();
    ScamValue newExp = makeInteger(33, true);
    engine.rebind(key, newExp);
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

    EXPECT_TRUE (engine.hasBinding(key));
    EXPECT_FALSE(engine.hasBinding(key2));
}

TEST_F(EnvTest, CheckCurrentOnly)
{
    EXPECT_TRUE(engine.hasBinding(key, false));
    engine.pushFrame();
    EXPECT_FALSE(engine.hasBinding(key, false));
}

TEST_F(EnvTest, NullKey)
{
    EXPECT_THROW(engine.addBinding(nullptr, exp), ScamException);
    EXPECT_THROW(engine.hasBinding(nullptr), ScamException);
    EXPECT_THROW(engine.getBinding(nullptr), ScamException);
    EXPECT_THROW(engine.rebind(nullptr, exp), ScamException);
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
