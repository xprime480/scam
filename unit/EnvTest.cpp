
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
        reset(false);
    }

    void reset(bool init)
    {
        engine.reset(init);
        engine.addBinding(key.get(), exp.get());
    }
};

TEST_F(EnvTest, Fetch)
{
    ExprHandle act = engine.getBinding(key.get());
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, FetchTraversesFrames)

{
    for ( size_t i = 0 ; i < 5 ; ++i ) {
        engine.pushFrame();
    }

    ExprHandle act = engine.getBinding(key.get());
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, DuplicateKeys)
{
    ExprHandle val2 = ExpressionFactory::makeInteger(2);
    EXPECT_THROW(engine.addBinding(key.get(), val2.get()), ScamException);

    ExprHandle act = engine.getBinding(key.get());
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, ExtensionTest)
{
    engine.pushFrame();
    ExprHandle exp2 = ExpressionFactory::makeInteger(2);
    engine.addBinding(key.get(), exp2.get());

    ExprHandle act2 = engine.getBinding(key.get());
    EXPECT_EQ(exp2->toInteger(), act2->toInteger());

    // original environment is unchanged
    //
    engine.popFrame();
    ExprHandle act = engine.getBinding(key.get());
    EXPECT_EQ(exp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Assign)
{
    ExprHandle newExp = ExpressionFactory::makeInteger(33);
    engine.rebind(key.get(), newExp.get());
    ExprHandle act = engine.getBinding(key.get());
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, AssignToNonexistentKey)
{
    ExprHandle newKey = ExpressionFactory::makeSymbol("*bad*");
    ExprHandle newExp = ExpressionFactory::makeInteger(33);
    EXPECT_THROW(engine.rebind(newKey.get(), newExp.get()), ScamException);
}

TEST_F(EnvTest, AssignTraversesFrames)
{
    engine.pushFrame();
    ExprHandle newExp = ExpressionFactory::makeInteger(33);
    engine.rebind(key.get(), newExp.get());
    ExprHandle act = engine.getBinding(key.get());
    EXPECT_EQ(newExp->toInteger(), act->toInteger());

    // it's in the original env
    //
    engine.popFrame();
    act = engine.getBinding(key.get());
    EXPECT_EQ(newExp->toInteger(), act->toInteger());
}

TEST_F(EnvTest, Check)
{
    ExprHandle key2 = ExpressionFactory::makeSymbol("bad");

    EXPECT_TRUE (engine.hasBinding(key.get()));
    EXPECT_FALSE(engine.hasBinding(key2.get()));
}

TEST_F(EnvTest, NonSymbolKey)
{
    EXPECT_THROW(engine.addBinding(exp.get(), exp.get()), ScamException);
    EXPECT_THROW(engine.hasBinding(exp.get()), ScamException);
    EXPECT_THROW(engine.getBinding(exp.get()), ScamException);
    EXPECT_THROW(engine.rebind(exp.get(), exp.get()), ScamException);
}

TEST_F(EnvTest, DefineConstant)
{
    reset(true);
    ExprHandle expr = parseAndEvaluate("(define x 1)");
    expectInteger(expr, 1, "1");

    ExprHandle sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = engine.getBinding(sym.get());
    expectInteger(val, 1, "1");
}

TEST_F(EnvTest, DefineEvaluated)
{
    reset(true);
    ExprHandle expr = parseAndEvaluate("(define x (- 3 2))");
    expectInteger(expr, 1, "1");

    ExprHandle sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = engine.getBinding(sym.get());
    expectInteger(val, 1, "1");
}

TEST_F(EnvTest, DefineScope)
{
    engine.pushFrame();
    ExprHandle expr = parseAndEvaluate("(define x 1)");
    engine.popFrame();
    ExprHandle sym = ExpressionFactory::makeSymbol("x");
    EXPECT_FALSE(engine.hasBinding(sym.get()));
    EXPECT_THROW(engine.getBinding(sym.get()), ScamException);
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
    expectInteger(expr, 77, "77");
}

TEST_F(EnvTest, AssignScope)
{
    reset(true);
    ExprHandle expr = parseAndEvaluateFile("scripts/env/assignscope.scm");
    expectInteger(expr, 77, "77");
}

TEST_F(EnvTest, GetTopLevel)
{
    reset(true);
    parseAndEvaluate("(define x 1)");
    engine.pushFrame();
    parseAndEvaluate("(define x 2)");

    ExprHandle sym = ExpressionFactory::makeSymbol("x");
    ExprHandle val = engine.getBinding(sym.get(), true);
    expectInteger(val, 1, "1");
}

