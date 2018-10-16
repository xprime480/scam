
#include "Env.hpp"

#include "ExpressionTestBase.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include "gtest/gtest.h"

#include <memory>

using namespace std;
using namespace scam;

class EnvTest : public ExpressionTestBase
{
protected:
    ExprHandle key;
    ExprHandle exp;

    EnvTest()
        : key(ExpressionFactory::makeSymbol("key"))
        , exp(ExpressionFactory::makeInteger(1))
    {
    }

    void SetUp() override
    {
        ExpressionTestBase::SetUp();
        env.put(key.get(), exp.get());
    }
};

TEST_F(EnvTest, Fetch)
{
    ExprHandle act = env.get(key.get());
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, FetchTraversesFrames)

{
    for ( size_t i = 0 ; i < 5 ; ++i ) {
        env = env.extend();
    }

    ExprHandle act = env.get(key.get());
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, DuplicateKeys)
{
    ExprHandle val2 = ExpressionFactory::makeInteger(2);
    EXPECT_THROW(env.put(key.get(), val2.get()), ScamException);

    ExprHandle act = env.get(key.get());
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, ExtensionTest)
{
    Env env2 = env.extend();
    ExprHandle exp2 = ExpressionFactory::makeInteger(2);
    env2.put(key.get(), exp2.get());

    ExprHandle act2 = env2.get(key.get());
    EXPECT_EQ(exp2->toInteger(), act2->toInteger());

    // original environment is unchanged
    //
    ExprHandle act = env.get(key.get());
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Assign)
{
    ExprHandle newExp = ExpressionFactory::makeInteger(33);
    env.assign(key.get(), newExp.get());
    ExprHandle act = env.get(key.get());
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, AssignToNonexistentKey)
{
    ExprHandle newKey = ExpressionFactory::makeSymbol("*bad*");
    ExprHandle newExp = ExpressionFactory::makeInteger(33);
    EXPECT_THROW(env.assign(newKey.get(), newExp.get()), ScamException);
}

TEST_F(EnvTest, AssignTraversesFrames)
{
    Env env2 = env.extend();
    ExprHandle newExp = ExpressionFactory::makeInteger(33);
    env2.assign(key.get(), newExp.get());
    ExprHandle act = env2.get(key.get());
    EXPECT_EQ(newExp->toInteger(), act->toInteger());

    // it's in the original env
    //
    act = env.get(key.get());
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Check)
{
    ExprHandle key2 = ExpressionFactory::makeSymbol("bad");

    EXPECT_TRUE(env.check(key.get()));
    EXPECT_FALSE(env.check(key2.get()));
}

TEST_F(EnvTest, NonSymbolKey)
{
    EXPECT_THROW(env.put(exp.get(), exp.get()), ScamException);
    EXPECT_THROW(env.check(exp.get()), ScamException);
    EXPECT_THROW(env.get(exp.get()), ScamException);
    EXPECT_THROW(env.assign(exp.get(), exp.get()), ScamException);
}

TEST_F(EnvTest, DefineConstant)
{
    ExprHandle expr = parseAndEvaluate("(define x 1)");
    expectInteger(expr, 1, "1");

    ExprHandle sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = env.get(sym.get());
    expectInteger(val, 1, "1");
}

TEST_F(EnvTest, DefineEvaluated)
{
    ExprHandle expr = parseAndEvaluate("(define x (- 3 2))");
    expectInteger(expr, 1, "1");

    ExprHandle sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = env.get(sym.get());
    expectInteger(val, 1, "1");
}

TEST_F(EnvTest, DefineScope)
{
    env = env.extend();
    ExprHandle expr = parseAndEvaluate("(define x 1)");
    env = env.parent();
    ExprHandle sym = ExpressionFactory::makeSymbol("x");
    EXPECT_THROW(env.get(sym.get()), ScamException);
}

TEST_F(EnvTest, DefineTwice)
{
    parseAndEvaluate("(define x 1)");
    EXPECT_THROW(parseAndEvaluate("(define x 2)"), ScamException);
}

TEST_F(EnvTest, AssignKeyword)
{
    parseAndEvaluate("(define x (- 3 2))");
    parseAndEvaluate("(assign x 77)");

    ExprHandle sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = env.get(sym.get());
    expectInteger(val, 77, "77");
}

TEST_F(EnvTest, AssignScope)
{
    parseAndEvaluate("(define x (- 3 2))");

    env = env.extend();
    parseAndEvaluate("(assign x 77)");
    env = env.parent();

    ExprHandle sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = env.get(sym.get());
    expectInteger(val, 77, "77");
}

