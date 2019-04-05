
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
    ScamExpr * key;
    ScamExpr * exp;

    EnvTest()
        : key(ExpressionFactory::makeSymbol("key"))
        , exp(ExpressionFactory::makeInteger(1))
    {
    }

    void SetUp() override
    {
        ExpressionTestBase::SetUp();
        reset(false);
    }

    void reset(bool init)
    {
        engine.reset(init);
        engine.addBinding(key, exp);
    }
};

TEST_F(EnvTest, Fetch)
{
    ScamExpr * act = engine.getBinding(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, FetchTraversesFrames)

{
    for ( size_t i = 0 ; i < 5 ; ++i ) {
        engine.pushFrame();
    }

    ScamExpr * act = engine.getBinding(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, DuplicateKeys)
{
    ScamExpr * val2 = ExpressionFactory::makeInteger(2);
    EXPECT_THROW(engine.addBinding(key, val2), ScamException);

    ScamExpr * act = engine.getBinding(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, ExtensionTest)
{
    engine.pushFrame();
    ScamExpr * exp2 = ExpressionFactory::makeInteger(2);
    engine.addBinding(key, exp2);

    ScamExpr * act2 = engine.getBinding(key);
    EXPECT_EQ(exp2->toInteger(), act2->toInteger());

    // original environment is unchanged
    //
    engine.popFrame();
    ScamExpr * act = engine.getBinding(key);
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Assign)
{
    ScamExpr * newExp = ExpressionFactory::makeInteger(33);
    engine.rebind(key, newExp);
    ScamExpr * act = engine.getBinding(key);
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, AssignToNonexistentKey)
{
    ScamExpr * newKey = ExpressionFactory::makeSymbol("*bad*");
    ScamExpr * newExp = ExpressionFactory::makeInteger(33);
    EXPECT_THROW(engine.rebind(newKey, newExp), ScamException);
}

TEST_F(EnvTest, AssignTraversesFrames)
{
    engine.pushFrame();
    ScamExpr * newExp = ExpressionFactory::makeInteger(33);
    engine.rebind(key, newExp);
    ScamExpr * act = engine.getBinding(key);
    EXPECT_EQ(newExp->toInteger(), act->toInteger());

    // it's in the original env
    //
    engine.popFrame();
    act = engine.getBinding(key);
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Check)
{
    ScamExpr * key2 = ExpressionFactory::makeSymbol("bad");

    EXPECT_TRUE (engine.hasBinding(key));
    EXPECT_FALSE(engine.hasBinding(key2));
}

TEST_F(EnvTest, CheckCurrentOnly)
{
    EXPECT_TRUE(engine.hasBinding(key, false));
    engine.pushFrame();
    EXPECT_FALSE(engine.hasBinding(key, false));
}

TEST_F(EnvTest, NonSymbolKey)
{
    EXPECT_THROW(engine.addBinding(exp, exp), ScamException);
    EXPECT_THROW(engine.hasBinding(exp), ScamException);
    EXPECT_THROW(engine.getBinding(exp), ScamException);
    EXPECT_THROW(engine.rebind(exp, exp), ScamException);
}

TEST_F(EnvTest, DefineConstant)
{
    reset(true);
    ScamExpr * expr = parseAndEvaluate("(define x 1)");
    expectSymbol(expr, "x");

    ScamExpr * sym = ExpressionFactory::makeSymbol("x");
    ScamExpr * val = engine.getBinding(sym);
    expectInteger(val, 1, "1");
}

TEST_F(EnvTest, DefineEvaluated)
{
    reset(true);
    ScamExpr * expr = parseAndEvaluate("(define x (- 3 2))");
    expectSymbol(expr, "x");

    ScamExpr * sym = ExpressionFactory::makeSymbol("x");
    ScamExpr * val = engine.getBinding(sym);
    expectInteger(val, 1, "1");
}

TEST_F(EnvTest, DefineScope)
{
    engine.pushFrame();
    (void) parseAndEvaluate("(define x 1)");
    engine.popFrame();
    ScamExpr * sym = ExpressionFactory::makeSymbol("x");
    EXPECT_FALSE(engine.hasBinding(sym));
    EXPECT_THROW(engine.getBinding(sym), ScamException);
}

TEST_F(EnvTest, DefineTwice)
{
    ScamExpr * expr = parseAndEvaluateFile("scripts/env/definetwice.scm");
    expectError(expr);
}

TEST_F(EnvTest, AssignKeyword)
{
    reset(true);
    ScamExpr * expr = parseAndEvaluateFile("scripts/env/assign.scm");
    expectInteger(expr, 77, "77");
}

TEST_F(EnvTest, AssignScope)
{
    reset(true);
    ScamExpr * expr = parseAndEvaluateFile("scripts/env/assignscope.scm");
    expectInteger(expr, 77, "77");
}

TEST_F(EnvTest, GetTopLevel)
{
    reset(true);
    parseAndEvaluate("(define x 1)");
    engine.pushFrame();
    parseAndEvaluate("(define x 2)");

    ScamExpr * sym = ExpressionFactory::makeSymbol("x");
    ScamExpr * val = engine.getBinding(sym, true);
    expectInteger(val, 1, "1");
}

TEST_F(EnvTest, Undefine)
{
    reset(true);

    ScamExpr * val = parseAndEvaluate("(define test 1) test");
    expectInteger(val, 1, "1");

    val = parseAndEvaluate("(undefine test)  test");
    expectError(val);
}

TEST_F(EnvTest, UndefineOnlyAffectsCurrentFrame)
{
    reset(true);

    ScamExpr * val = parseAndEvaluate("(define test 1) test");
    expectInteger(val, 1, "1");

    engine.pushFrame();

    val = parseAndEvaluate("(undefine test)  test");
    expectInteger(val, 1, "1");
}

