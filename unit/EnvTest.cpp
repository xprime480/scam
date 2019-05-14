#include "Env.hpp"

#include "TestBase.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

class EnvTest : public TestBase
{
protected:
    ScamSymbol * key;
    ExprHandle   exp;

    EnvTest()
        : key(nullptr)
        , exp(nullptr)
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

        key = ExpressionFactory::makeSymbol("key");
        exp = ExpressionFactory::makeInteger(1, true);

        engine.addBinding(key, exp);
    }
};

TEST_F(EnvTest, Fetch)
{
    ExprHandle act = engine.getBinding(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, FetchTraversesFrames)

{
    for ( size_t i = 0 ; i < 5 ; ++i ) {
        engine.pushFrame();
    }

    ExprHandle act = engine.getBinding(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, DuplicateKeys)
{
    ExprHandle val2 = ExpressionFactory::makeInteger(2, true);
    EXPECT_THROW(engine.addBinding(key, val2), ScamException);

    ExprHandle act = engine.getBinding(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, ExtensionTest)
{
    engine.pushFrame();
    ExprHandle exp2 = ExpressionFactory::makeInteger(2, true);
    engine.addBinding(key, exp2);

    ExprHandle act2 = engine.getBinding(key);
    EXPECT_EQ(exp2->toInteger(), act2->toInteger());

    // original environment is unchanged
    //
    engine.popFrame();
    ExprHandle act = engine.getBinding(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Assign)
{
    ExprHandle newExp = ExpressionFactory::makeInteger(33, true);
    engine.rebind(key, newExp);
    ExprHandle act = engine.getBinding(key);
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, AssignToNonexistentKey)
{
    ScamSymbol * newKey = ExpressionFactory::makeSymbol("*bad*");
    ExprHandle   newExp = ExpressionFactory::makeInteger(33, true);
    EXPECT_THROW(engine.rebind(newKey, newExp), ScamException);
}

TEST_F(EnvTest, AssignTraversesFrames)
{
    engine.pushFrame();
    ExprHandle newExp = ExpressionFactory::makeInteger(33, true);
    engine.rebind(key, newExp);
    ExprHandle act = engine.getBinding(key);
    EXPECT_EQ(newExp->toInteger(), act->toInteger());

    // it's in the original env
    //
    engine.popFrame();
    act = engine.getBinding(key);
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Check)
{
    ScamSymbol * key2 = ExpressionFactory::makeSymbol("bad");

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

TEST_F(EnvTest, DefineConstant)
{
    reset(true);
    ExprHandle expr = parseAndEvaluate("(define x 1)");
    expectNull(expr);

    ScamSymbol * sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = engine.getBinding(sym);
    expectInteger(val, 1, "1", true);
}

TEST_F(EnvTest, DefineEvaluated)
{
    reset(true);
    ExprHandle expr = parseAndEvaluate("(define x (- 3 2))");
    expectNull(expr);

    ScamSymbol * sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = engine.getBinding(sym);
    expectInteger(val, 1, "1", true);
}

TEST_F(EnvTest, DefineScope)
{
    engine.pushFrame();
    (void) parseAndEvaluate("(define x 1)");
    engine.popFrame();
    ScamSymbol * sym = ExpressionFactory::makeSymbol("x");
    EXPECT_FALSE(engine.hasBinding(sym));
    EXPECT_THROW(engine.getBinding(sym), ScamException);
}

TEST_F(EnvTest, DefineTwice)
{
    ExprHandle expr = parseAndEvaluateFile("scripts/env/definetwice.scm");
    expectError(expr);
}

TEST_F(EnvTest, AssignKeyword)
{
    reset(true);
    ExprHandle expr = parseAndEvaluateFile("scripts/env/assign.scm");
    expectInteger(expr, 77, "77", true);
}

TEST_F(EnvTest, AssignScope)
{
    reset(true);
    ExprHandle expr = parseAndEvaluateFile("scripts/env/assignscope.scm");
    expectInteger(expr, 77, "77", true);
}

TEST_F(EnvTest, GetTopLevel)
{
    reset(true);
    parseAndEvaluate("(define x 1)");
    engine.pushFrame();
    parseAndEvaluate("(define x 2)");

    ScamSymbol * sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = engine.getBinding(sym, true);
    expectInteger(val, 1, "1", true);
}

TEST_F(EnvTest, Undefine)
{
    reset(true);

    ExprHandle val = parseAndEvaluate("(define test 1) test");
    expectInteger(val, 1, "1", true);

    val = parseAndEvaluate("(undefine test)  test");
    expectError(val);
}

TEST_F(EnvTest, UndefineOnlyAffectsCurrentFrame)
{
    reset(true);

    ExprHandle val = parseAndEvaluate("(define test 1) test");
    expectInteger(val, 1, "1", true);

    engine.pushFrame();

    val = parseAndEvaluate("(undefine test)  test");
    expectInteger(val, 1, "1", true);
}
