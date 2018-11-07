
#include "ExpressionTestBase.hpp"

#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"

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
    static const unsigned long SELECT_PROC    { 1 << 15 };

    static const unsigned long ALL_FLOAT   = SELECT_NUMERIC | SELECT_FLOAT;
    static const unsigned long ALL_INTEGER = ALL_FLOAT | SELECT_INTEGER;
    static const unsigned long ALL_NIL     = SELECT_NIL | SELECT_LIST;
    static const unsigned long ALL_PROC    = SELECT_APPLY | SELECT_PROC;
}

ExpressionTestBase::ExpressionTestBase()
{
}

ExpressionTestBase::~ExpressionTestBase()
{
}

void ExpressionTestBase::SetUp()
{
    extractor = make_shared<Extractor>();
    try {
        env = ScamEngine::getStandardEnv();
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "creating standard env";
    }
}

void ExpressionTestBase::TearDown()
{
}

ExprHandle ExpressionTestBase::evaluate(ExprHandle input)
{
    auto y = make_shared<Extractor>();
    input->eval(y, env);
    Trampoline(GlobalWorkQueue);
    return y->getExpr();
}

ExprHandle ExpressionTestBase::apply(ExprHandle expr, ExprHandle args)
{
    expr->apply(args.get(), extractor, env);
    Trampoline(GlobalWorkQueue);
    return extractor->getExpr();
}

ExprHandle ExpressionTestBase::parseAndEvaluate(string const & input)
{
    try {
        StringTokenizer tokenizer(input);
        ScamParser parser(tokenizer);

        parser.parseExpr(extractor);
        ExprHandle expr = extractor->getExpr();

        ExprHandle rv = evaluate(expr);
        return rv;
    }
    catch ( ScamException e ) {
        return ExpressionFactory::makeError(e.getMessage());
    }
}

void decodeBit(unsigned mismatch,
               unsigned exp,
               unsigned sel,
               char const * tag,
               stringstream & s)
{
    if ( mismatch & sel ) {
        if ( exp & sel ) {
            s << "\n\tUnexpected " << tag << " flag set";
        }
        else {
            s << "\n\tExpected " << tag << " flag not set";
        }
    }
}

string decodePredicate(unsigned exp, unsigned act)
{
    stringstream s;

    if ( exp != act ) {
        unsigned mismatch = (exp ^ act);

#define DECODER(B) \
        decodeBit(mismatch, exp, SELECT_ ## B, #B, s)

        DECODER(NULL);
        DECODER(ERROR);
        DECODER(TRUTH);

        DECODER(CHAR);
        DECODER(STRING);
        DECODER(SYMBOL);

        DECODER(NUMERIC);
        DECODER(FLOAT);
        DECODER(BOOLEAN);

        DECODER(NIL);
        DECODER(CONS);
        DECODER(LIST);
        DECODER(VECTOR);

        DECODER(APPLY);
        DECODER(PROC);
#undef DECODER
    }

    return s.str();
}

void
ExpressionTestBase::checkPredicates(ExprHandle expr, unsigned exp)
{
    ASSERT_NE(nullptr, expr.get());
    unsigned act { 0 };

    act |= (expr->isNull() ? SELECT_NULL : 0);
    act |= (expr->error() ? SELECT_ERROR : 0);
    act |= (expr->truth() ? SELECT_TRUTH : 0);

    act |= (expr->isBoolean() ? SELECT_BOOLEAN : 0);
    act |= (expr->isChar() ? SELECT_CHAR : 0);
    act |= (expr->isString() ? SELECT_STRING : 0);
    act |= (expr->isSymbol() ? SELECT_SYMBOL : 0);

    act |= (expr->isNumeric() ? SELECT_NUMERIC : 0);
    act |= (expr->isFloat() ? SELECT_FLOAT : 0);
    act |= (expr->isInteger() ? SELECT_INTEGER : 0);

    act |= (expr->isNil() ? SELECT_NIL : 0);
    act |= (expr->isCons() ? SELECT_CONS : 0);
    act |= (expr->isList() ? SELECT_LIST : 0);
    act |= (expr->isVector() ? SELECT_VECTOR : 0);

    act |= (expr->hasApply() ? SELECT_APPLY : 0);
    act |= (expr->isProcedure() ? SELECT_PROC : 0);

    EXPECT_EQ(exp, act) << decodePredicate(act, exp)
                        << "\n\tvalue: " << expr->toString();
}

void expectNonNumeric(ExprHandle expr)
{
    EXPECT_THROW(expr->toFloat(), ScamException) << "got " << expr->toFloat();
    EXPECT_THROW(expr->toInteger(), ScamException) << "got "  << expr->toInteger();
}

void ExpressionTestBase::expectNull(ExprHandle expr)
{
    checkPredicates(expr, SELECT_NULL);
    EXPECT_EQ("null", expr->toString());

    expectNonNumeric(expr);
}

void ExpressionTestBase::expectError(ExprHandle expr, string const msg)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_ERROR );

    if ( ! msg.empty() ) {
        EXPECT_EQ(msg, expr->toString());
    }

    expectNonNumeric(expr);
}

void ExpressionTestBase::expectBoolean(ExprHandle expr,
                                       bool value,
                                       string const & repr)
{
    EXPECT_EQ(repr, expr->toString());
    checkPredicates(expr, SELECT_BOOLEAN | (value ? SELECT_TRUTH : 0));

    expectNonNumeric(expr);
}

void ExpressionTestBase::booleanTest(ExprHandle expr,
                                     bool value,
                                     string const & repr)
{
    expectBoolean(expr, value, repr);
    ExprHandle evaled = evaluate(expr);
    expectBoolean(evaled, value, repr);
}

void ExpressionTestBase::expectFloat(ExprHandle expr,
                                     double value,
                                     string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_FLOAT);
    EXPECT_EQ(repr, expr->toString());

    try {
        EXPECT_EQ(value, expr->toFloat());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }

    EXPECT_THROW(expr->toInteger(), ScamException);
}

void ExpressionTestBase::expectInteger(ExprHandle expr,
                                       int value,
                                       string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ(repr, expr->toString());
    try {
        EXPECT_EQ((double)value, expr->toFloat());
        EXPECT_EQ(value, expr->toInteger());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }
}

void
ExpressionTestBase::expectChar(ExprHandle expr, char value, string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_CHAR);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(value, expr->toChar());
}

void ExpressionTestBase::expectString(ExprHandle expr, string const & value)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_STRING);
    EXPECT_EQ(value, expr->toString());
}

void ExpressionTestBase::expectSymbol(ExprHandle expr, string const & name)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_SYMBOL);
    EXPECT_EQ(name, expr->toString());
}

void ExpressionTestBase::expectNil(ExprHandle expr)
{
    static const string repr { "()" };
    checkPredicates(expr, SELECT_TRUTH | ALL_NIL);
    EXPECT_EQ(repr, expr->toString());
}

void ExpressionTestBase::expectList(ExprHandle expr,
                                    string const & repr,
                                    size_t len)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS | SELECT_LIST);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(len, expr->length());
}

void ExpressionTestBase::expectCons(ExprHandle expr, string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS);
    EXPECT_EQ(repr, expr->toString());
}

void ExpressionTestBase::expectApplicable(ExprHandle expr, string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_APPLY);
    EXPECT_EQ(repr, expr->toString());
}

void ExpressionTestBase::expectVector(ExprHandle expr,
                                      string const & repr,
                                      size_t len)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_VECTOR);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(len, expr->length());
}

void ExpressionTestBase::expectProcedure(ExprHandle expr, string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_PROC);
    EXPECT_EQ(repr, expr->toString());
}
