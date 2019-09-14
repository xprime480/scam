#include "TestBase.hpp"

#include "ErrorCategory.hpp"
#include "env/Env.hpp"
#include "value/ValueFactory.hpp"
#include "value/ScamData.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

class EnvTest : public TestBase
{
protected:
    Env * env;
    ScamValue key;
    ScamValue exp;

    EnvTest()
        : env(mm.make<Env>())
        , key(makeNothing())
        , exp(makeNothing())
    {
        key = makeSymbol("key");
        exp = makeInteger(1, true);
        expectNothing(env->put(key, exp));
    }
};

TEST_F(EnvTest, Fetch)
{
    ScamValue act = env->get(key);
    EXPECT_EQ(asInteger(exp), asInteger(act));
}

TEST_F(EnvTest, FetchTraversesFrames)
{
    for ( size_t i = 0 ; i < 5 ; ++i ) {
        env = env->extend();
    }

    ScamValue act = env->get(key);
    EXPECT_EQ(asInteger(exp), asInteger(act));
}

TEST_F(EnvTest, DuplicateKeys)
{
    ScamValue val2 = makeInteger(2, true);
    ScamValue expr = env->put(key, val2);
    expectError(expr);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());

    ScamValue act = env->get(key);
    EXPECT_EQ(asInteger(exp), asInteger(act));
}

TEST_F(EnvTest, ExtensionTest)
{
    env = env->extend();
    ScamValue exp2 = makeInteger(2, true);
    expectNothing(env->put(key, exp2));

    ScamValue act2 = env->get(key);
    EXPECT_EQ(asInteger(exp2), asInteger(act2));

    // original environment is unchanged
    //
    env = env->getParent();
    ScamValue act = env->get(key);
    EXPECT_EQ(asInteger(exp), asInteger(act));
}

TEST_F(EnvTest, Assign)
{
    ScamValue newExp = makeInteger(33, true);
    expectNothing(env->assign(key, newExp));
    ScamValue act = env->get(key);
    EXPECT_EQ(asInteger(newExp), asInteger(act));
}

TEST_F(EnvTest, AssignToNonexistentKey)
{
    ScamValue newKey = makeSymbol("*bad*");
    ScamValue newExp = makeInteger(33, true);
    ScamValue test = env->assign(newKey, newExp);
    ASSERT_TRUE(isError(test));
    EXPECT_EQ(envCategory, test->errorCategory());
}

TEST_F(EnvTest, AssignTraversesFrames)
{
    env = env->extend();
    ScamValue newExp = makeInteger(33, true);
    expectNothing(env->assign(key, newExp));
    ScamValue act = env->get(key);
    EXPECT_EQ(asInteger(newExp), asInteger(act));

    // it's in the original env
    //
    env = env->getParent();
    act = env->get(key);
    EXPECT_EQ(asInteger(newExp), asInteger(act));
}

TEST_F(EnvTest, Check)
{
    ScamValue key2 = makeSymbol("bad");

    expectBoolean(env->check(key),  true,  "#t");
    expectBoolean(env->check(key2), false, "#f");
}

TEST_F(EnvTest, CheckCurrentOnly)
{
    expectBoolean(env->check(key, false), true, "#t");
    env = env->extend();
    expectBoolean(env->check(key, false), false, "#f");
}

TEST_F(EnvTest, NullKey)
{
    ScamValue expr = env->put(nullptr, exp);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());

    expr = env->check(nullptr);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());

    expr = env->get(nullptr);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());

    expr = env->assign(nullptr, exp);
    ASSERT_TRUE(isUnhandledError(expr));
    EXPECT_EQ(envCategory, expr->errorCategory());
}

TEST_F(EnvTest, MergeTest)
{
    Env * env1 = mm.make<Env>();
    Env * env2 = mm.make<Env>();

    ScamValue sym = makeSymbol("x");
    ScamValue val = makeInteger(1, true);

    env2->put(sym, val);
    env1->merge(env2);
    ScamValue rv = env1->get(sym);
    expectInteger(rv, 1, "1", true);
}

TEST_F(EnvTest, GetKeys)
{
    Env * env = mm.make<Env>();

    ScamValue symX = makeSymbol("x");
    ScamValue symY = makeSymbol("y");
    ScamValue val = makeInteger(1, true);

    env->put(symX, val);
    env->put(symY, val);

    set<string> keys;
    env->getKeys(keys);

    EXPECT_EQ(2, keys.size());
    EXPECT_FALSE(keys.find("x") == keys.end());
    EXPECT_FALSE(keys.find("y") == keys.end());
    EXPECT_TRUE(keys.find("z") == keys.end());
}

TEST_F(EnvTest, Forwarding)
{
    Env * env = mm.make<Env>();
    Env * proxy = mm.make<Env>();

    ScamValue symX = makeSymbol("x");
    ScamValue val1 = makeInteger(1, true);
    ScamValue val2 = makeInteger(2, true);
    ScamValue fwd  = makeForwarder(env);

    env->put(symX, val1);
    proxy->put(symX, fwd);
    expectBoolean(env->check(symX), true, "#t");
    expectBoolean(proxy->check(symX), true, "#t");
    expectInteger(proxy->get(symX), 1, "1", true);

    proxy->assign(symX, val2);
    expectInteger(env->get(symX), 2, "2", true);

    env->remove(symX);
    expectBoolean(env->check(symX), false, "#f");
    expectBoolean(proxy->check(symX), false, "#f");
}
