
#include "ExpressionTestBase.hpp"

#include "ScamException.hpp"
#include "Trampoline.hpp"
#include "WorkQueue.hpp"

using namespace scam;
using namespace std;

namespace
{
    static const unsigned long SELECT_NULL    { 1 << 0 };
    static const unsigned long SELECT_ERROR   { 1 << 1 };
    static const unsigned long SELECT_TRUTH   { 1 << 2 };
    static const unsigned long SELECT_CHAR    { 1 << 3 };
    static const unsigned long SELECT_STRING  { 1 << 4 };
    static const unsigned long SELECT_SYMBOL  { 1 << 5 };
    static const unsigned long SELECT_NUMERIC { 1 << 6 };
    static const unsigned long SELECT_FLOAT   { 1 << 7 };
    static const unsigned long SELECT_INTEGER { 1 << 8 };
    static const unsigned long SELECT_BOOLEAN { 1 << 9 };
    static const unsigned long SELECT_NIL     { 1 << 10 };
    static const unsigned long SELECT_CONS    { 1 << 11 };
    static const unsigned long SELECT_LIST    { 1 << 12 };
    static const unsigned long SELECT_VECTOR  { 1 << 13 };
    static const unsigned long SELECT_APPLY   { 1 << 14 };

    static const unsigned long ALL_FLOAT   = SELECT_NUMERIC | SELECT_FLOAT;
    static const unsigned long ALL_INTEGER = ALL_FLOAT | SELECT_INTEGER;
    static const unsigned long ALL_NIL     = SELECT_NIL | SELECT_LIST;
}

ExpressionTestBase::ExpressionTestBase()
    : extractor(make_shared<Extractor>())
{
}

shared_ptr<ScamExpr> ExpressionTestBase::evaluate(shared_ptr<ScamExpr> input)
{
    input->eval(extractor, env);
    Trampoline(GlobalWorkQueue);
    return extractor->getExpr();
}

void ExpressionTestBase::doCheck(bool act, unsigned selector, unsigned which)
{
    bool exp = (selector & which) == which;
    EXPECT_EQ(exp, act) << "failed for " << which;
}

void
ExpressionTestBase::checkPredicates(shared_ptr<ScamExpr> expr, unsigned selector)
{
    ASSERT_NE(nullptr, expr.get());

    doCheck(expr->isNull(), selector, SELECT_NULL);
    doCheck(expr->error(),  selector, SELECT_ERROR);
    doCheck(expr->truth(),  selector, SELECT_TRUTH);

    doCheck(expr->isBoolean(), selector, SELECT_BOOLEAN);
    doCheck(expr->isChar(),    selector, SELECT_CHAR);
    doCheck(expr->isString(),  selector, SELECT_STRING);
    doCheck(expr->isSymbol(),  selector, SELECT_SYMBOL);

    doCheck(expr->isNumeric(), selector, SELECT_NUMERIC);
    doCheck(expr->isFloat(),   selector, SELECT_FLOAT);
    doCheck(expr->isInteger(), selector, SELECT_INTEGER);

    doCheck(expr->isNil(),    selector, SELECT_NIL);
    doCheck(expr->isCons(),   selector, SELECT_CONS);
    doCheck(expr->isList(),   selector, SELECT_LIST);
    doCheck(expr->isVector(), selector, SELECT_VECTOR);

    doCheck(expr->hasApply(), selector, SELECT_APPLY);
}

void ExpressionTestBase::expectNull(shared_ptr<ScamExpr> expr)
{
    checkPredicates(expr, SELECT_NULL);
    EXPECT_EQ("null", expr->toString());

    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);
}

void ExpressionTestBase::expectError(shared_ptr<ScamExpr> expr,
				     string const msg)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_ERROR );

    if ( ! msg.empty() ) {
	EXPECT_EQ(msg, expr->toString());
    }

    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);
}

void ExpressionTestBase::expectBoolean(shared_ptr<ScamExpr> expr, 
				       bool value, 
				       string const & repr)
{
    EXPECT_EQ(repr, expr->toString());
    checkPredicates(expr, SELECT_BOOLEAN | (value ? SELECT_TRUTH : 0));

    EXPECT_THROW(expr->toFloat(), ScamException);
    EXPECT_THROW(expr->toInteger(), ScamException);
}

void ExpressionTestBase::booleanTest(shared_ptr<ScamExpr> expr, 
				     bool value, 
				     string const & repr)
{
    expectBoolean(expr, value, repr);
    shared_ptr<ScamExpr> evaled = evaluate(expr);
    expectBoolean(evaled, value, repr);
}

void ExpressionTestBase::expectFloat(shared_ptr<ScamExpr> expr, 
				     double value, 
				     string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_FLOAT);
    EXPECT_EQ(repr, expr->toString());

    EXPECT_EQ(value, expr->toFloat());
    EXPECT_THROW(expr->toInteger(), ScamException);
}

void ExpressionTestBase::expectInteger(shared_ptr<ScamExpr> expr, 
				       int value, 
				       string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ((double)value, expr->toFloat());
    EXPECT_EQ(value, expr->toInteger());
}

void ExpressionTestBase::expectChar(shared_ptr<ScamExpr> expr, 
				    char value, 
				    string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_CHAR);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(value, expr->toChar());
}

void ExpressionTestBase::expectString(shared_ptr<ScamExpr> expr, 
				      string const & value)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_STRING);
    EXPECT_EQ(value, expr->toString());
}

void ExpressionTestBase::expectSymbol(shared_ptr<ScamExpr> expr, 
				      string const & name)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_SYMBOL);
    EXPECT_EQ(name, expr->toString());
}

void ExpressionTestBase::expectNil(shared_ptr<ScamExpr> expr, 
				   string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_NIL);
    EXPECT_EQ(repr, expr->toString());
}

void ExpressionTestBase::expectList(shared_ptr<ScamExpr> expr, 
				    string const & repr, 
				    size_t len)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS | SELECT_LIST);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(len, expr->length());
}

void ExpressionTestBase::expectCons(shared_ptr<ScamExpr> expr, 
				    string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS);
    EXPECT_EQ(repr, expr->toString());
}

void ExpressionTestBase::expectApplicable(shared_ptr<ScamExpr> expr, 
					  string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_APPLY);
    EXPECT_EQ(repr, expr->toString());
}

void ExpressionTestBase::expectVector(shared_ptr<ScamExpr> expr, 
				      string const & repr, 
				      size_t len)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_VECTOR);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(len, expr->length());
}
