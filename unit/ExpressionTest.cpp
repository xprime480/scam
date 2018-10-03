
#include "ScamContext.hpp"
#include "ScamException.hpp"

#include "expr/ExpressionFactory.hpp"

#include "Extractor.hpp"

#include "gtest/gtest.h"

using namespace std;
using namespace scam;

class ExpressionTest : public ::testing::Test
{
protected:
    ExpressionTest()
        : extractor(make_shared<Extractor>())
    {
        context.cont = extractor;
    }

    shared_ptr<Extractor> extractor;
    ScamContext context;

    std::shared_ptr<ScamExpr> evaluate(shared_ptr<ScamExpr> input)
    {
        input->eval(context);
        return extractor->getExpr();
    }

    void booleanTest(shared_ptr<ScamExpr> expr, bool value, string const & repr)
    {
        ASSERT_NE(nullptr, expr.get());

        EXPECT_EQ(repr, expr->toString());

        EXPECT_FALSE(expr->isNull());
        EXPECT_FALSE(expr->error());
        EXPECT_EQ(value, expr->truth());
        EXPECT_FALSE(expr->isNumeric());
        EXPECT_FALSE(expr->isFloat());
        EXPECT_FALSE(expr->isInteger());
        EXPECT_THROW(expr->toFloat(), ScamException);
        EXPECT_THROW(expr->toInteger(), ScamException);
        EXPECT_FALSE(expr->isString());
        EXPECT_FALSE(expr->isSymbol());

        shared_ptr<ScamExpr> evaled = evaluate(expr);
        ASSERT_NE(nullptr, evaled.get());
        EXPECT_EQ(expr.get(), evaled.get());
        EXPECT_EQ(value, evaled->truth());
    }

};

TEST_F(ExpressionTest, NullExpression)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeNull();

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ("null", expr->toString());

    EXPECT_TRUE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_FALSE(expr->truth());
    EXPECT_FALSE(expr->isNumeric());
    EXPECT_FALSE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);
    EXPECT_FALSE(expr->isString());
    EXPECT_FALSE(expr->isSymbol());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_TRUE(evaled->error());
}

TEST_F(ExpressionTest, ErrorExpression)
{
    string const msg("Test message");

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeError(msg);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(msg, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_TRUE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_FALSE(expr->isNumeric());
    EXPECT_FALSE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);
    EXPECT_FALSE(expr->isString());
    EXPECT_FALSE(expr->isSymbol());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toString(), evaled->toString());
}

TEST_F(ExpressionTest, BooleanTrue)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeBoolean(true);
    booleanTest(expr, true, "#t");
}

TEST_F(ExpressionTest, BooleanFalse)
{
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeBoolean(false);
    booleanTest(expr, false, "#f");
}

TEST_F(ExpressionTest, FloatTest)
{
    double value { 33.2 };
    string const repr{ "33.2" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeFloat(value);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(repr, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_TRUE(expr->isNumeric());
    EXPECT_TRUE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_EQ(value, expr->toFloat());
    EXPECT_THROW(expr->toInteger(), ScamException);
    EXPECT_FALSE(expr->isString());
    EXPECT_FALSE(expr->isSymbol());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toFloat(), evaled->toFloat());
}

TEST_F(ExpressionTest, IntegerTest)
{
    int value { 42 };
    string const repr{ "42" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeInteger(value);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(repr, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_TRUE(expr->isNumeric());
    EXPECT_TRUE(expr->isFloat());
    EXPECT_TRUE(expr->isInteger());
    EXPECT_EQ((double)value, expr->toFloat());
    EXPECT_EQ(value, expr->toInteger());
    EXPECT_FALSE(expr->isString());
    EXPECT_FALSE(expr->isSymbol());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toInteger(), evaled->toInteger());
}

TEST_F(ExpressionTest, CharacterTest)
{
    string const value { "\\#Q" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeCharacter(value);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(value, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_FALSE(expr->isNumeric());
    EXPECT_FALSE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_TRUE(expr->isChar());
    EXPECT_FALSE(expr->isString());
    EXPECT_FALSE(expr->isSymbol());
    EXPECT_EQ('Q', expr->toChar());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toChar(), evaled->toChar());
}

TEST_F(ExpressionTest, StringTest)
{
    string const value { "Fnord!" };

    shared_ptr<ScamExpr> expr = ExpressionFactory::makeString(value);

    ASSERT_NE(nullptr, expr.get());

    EXPECT_EQ(value, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_FALSE(expr->error());
    EXPECT_TRUE(expr->truth());
    EXPECT_FALSE(expr->isNumeric());
    EXPECT_FALSE(expr->isFloat());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_FALSE(expr->isChar());
    EXPECT_TRUE(expr->isString());
    EXPECT_FALSE(expr->isSymbol());
    EXPECT_EQ(value, expr->toString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toString(), evaled->toString());
}

TEST_F(ExpressionTest, SymbolTest)
{
    string const name { "Fnord!" };

    shared_ptr<ScamExpr> sym = ExpressionFactory::makeSymbol(name);

    ASSERT_NE(nullptr, sym.get());

    EXPECT_EQ(name, sym->toString());

    EXPECT_FALSE(sym->isNull());
    EXPECT_FALSE(sym->error());
    EXPECT_TRUE(sym->truth());
    EXPECT_FALSE(sym->isNumeric());
    EXPECT_FALSE(sym->isFloat());
    EXPECT_FALSE(sym->isInteger());
    EXPECT_FALSE(sym->isChar());
    EXPECT_FALSE(sym->isString());
    EXPECT_TRUE(sym->isSymbol());
    EXPECT_EQ(name, sym->toString());

    shared_ptr<ScamExpr> evaled = evaluate(sym);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_TRUE(evaled->error());

    shared_ptr<ScamExpr> value = ExpressionFactory::makeInteger(1899);
    context.env.put(sym, value);
    evaled = evaluate(sym);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_FALSE(evaled->error());
    EXPECT_EQ(value->toInteger(), evaled->toInteger());
}

TEST_F(ExpressionTest, NilTest)
{
    string const value { "()" };
    shared_ptr<ScamExpr> expr = ExpressionFactory::makeNil();

    ASSERT_NE(nullptr, expr.get());
    EXPECT_EQ(value, expr->toString());

    EXPECT_FALSE(expr->isNull());
    EXPECT_TRUE(expr->truth());
    EXPECT_TRUE(expr->isNil());
    EXPECT_EQ(value, expr->toString());

    shared_ptr<ScamExpr> evaled = evaluate(expr);
    ASSERT_NE(nullptr, evaled.get());
    EXPECT_EQ(expr->toString(), evaled->toString());
}
