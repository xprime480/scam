
#include "Env.hpp"

#include "ScamException.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include "gtest/gtest.h"

#include <memory>

using namespace std;
using namespace scam;

class EnvTest : public ::testing::Test
{
protected:
    shared_ptr<ScamExpr> key;
    shared_ptr<ScamExpr> exp;

    Env env;

    EnvTest()
        : key(ExpressionFactory::makeSymbol("key"))
        , exp(ExpressionFactory::makeInteger(1))
        , env(Env())
    {
        env.put(key, exp);
    }
};

TEST_F(EnvTest, Fetch)
{
    shared_ptr<ScamExpr> act = env.get(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, FetchTraversesFrames)

{
    for ( size_t i = 0 ; i < 5 ; ++i ) {
        env = env.extend();
    }

    shared_ptr<ScamExpr> act = env.get(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, DuplicateKeys)
{
    shared_ptr<ScamExpr> val2 = ExpressionFactory::makeInteger(2);
    EXPECT_THROW(env.put(key, val2), ScamException);

    shared_ptr<ScamExpr> act = env.get(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, ExtensionTest)
{
    Env env2 = env.extend();
    shared_ptr<ScamExpr> exp2 = ExpressionFactory::makeInteger(2);
    env2.put(key, exp2);

    shared_ptr<ScamExpr> act2 = env2.get(key);
    EXPECT_EQ(exp2->toInteger(), act2->toInteger());

    // original environment is unchanged
    //
    shared_ptr<ScamExpr> act = env.get(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Assign)
{
    shared_ptr<ScamExpr> newExp = ExpressionFactory::makeInteger(33);
    env.assign(key, newExp);
    shared_ptr<ScamExpr> act = env.get(key);
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, AssignToNonexistentKey)
{
    shared_ptr<ScamExpr> newKey = ExpressionFactory::makeSymbol("*bad*");
    shared_ptr<ScamExpr> newExp = ExpressionFactory::makeInteger(33);
    EXPECT_THROW(env.assign(newKey, newExp), ScamException);
}

TEST_F(EnvTest, AssignTraversesFrames)
{
    Env env2 = env.extend();
    shared_ptr<ScamExpr> newExp = ExpressionFactory::makeInteger(33);
    env2.assign(key, newExp);
    shared_ptr<ScamExpr> act = env2.get(key);
    EXPECT_EQ(newExp->toInteger(), act->toInteger());

    // it's in the original env
    //
    act = env.get(key);
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Check)
{
    shared_ptr<ScamExpr> key2 = ExpressionFactory::makeSymbol("bad");

    EXPECT_TRUE(env.check(key));
    EXPECT_FALSE(env.check(key2));
}
