#include "TestBase.hpp"

#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"
#include "util/ReadEvalString.hpp"

using namespace scam;
using namespace std;

namespace
{
    static const unsigned long SELECT_NULL     { 1 << 0 };
    static const unsigned long SELECT_ERROR    { 1 << 1 };
    static const unsigned long SELECT_TRUTH    { 1 << 2 };
    static const unsigned long SELECT_CHAR     { 1 << 3 };
    static const unsigned long SELECT_STRING   { 1 << 4 };
    static const unsigned long SELECT_SYMBOL   { 1 << 5 };
    static const unsigned long SELECT_NUMERIC  { 1 << 6 };
    static const unsigned long SELECT_COMPLEX  { 1 << 7 };
    static const unsigned long SELECT_REAL     { 1 << 8 };
    static const unsigned long SELECT_RATIONAL { 1 << 9 };
    static const unsigned long SELECT_INTEGER  { 1 << 10 };
    static const unsigned long SELECT_BOOLEAN  { 1 << 11 };
    static const unsigned long SELECT_NIL      { 1 << 12 };
    static const unsigned long SELECT_CONS     { 1 << 13 };
    static const unsigned long SELECT_LIST     { 1 << 14 };
    static const unsigned long SELECT_VECTOR   { 1 << 15 };
    static const unsigned long SELECT_APPLY    { 1 << 16 };
    static const unsigned long SELECT_PROC     { 1 << 17 };
    static const unsigned long SELECT_CLASS    { 1 << 18 };
    static const unsigned long SELECT_INSTANCE { 1 << 19 };
    static const unsigned long SELECT_KEYWORD  { 1 << 20 };
    static const unsigned long SELECT_DICT     { 1 << 21 };
    static const unsigned long SELECT_MANAGED  { 1 << 22 };

    static const unsigned long ALL_COMPLEX    = SELECT_NUMERIC | SELECT_COMPLEX | SELECT_MANAGED;
    static const unsigned long ALL_REAL       = ALL_COMPLEX | SELECT_REAL | SELECT_MANAGED;
    static const unsigned long ALL_RATIONAL   = ALL_REAL | SELECT_RATIONAL;
    static const unsigned long ALL_INTEGER    = ALL_RATIONAL | SELECT_INTEGER;
    static const unsigned long ALL_NIL        = SELECT_NIL | SELECT_LIST;
    static const unsigned long ALL_PROC       = SELECT_APPLY | SELECT_PROC | SELECT_MANAGED;
    static const unsigned long ALL_CLASS      = SELECT_CLASS | ALL_PROC;
    static const unsigned long ALL_INSTANCE   = SELECT_INSTANCE | ALL_PROC;
    static const unsigned long ALL_DICT       = SELECT_DICT | SELECT_APPLY | SELECT_MANAGED;
}

TestBase::TestBase(bool loadPrelude)
    : mm(standardMemoryManager)
{
    mm.reset();
    engine.reset(true);
    extractor = mm.make<Extractor>();
    engine.setCont(extractor);
    if ( loadPrelude ) {
        ExprHandle result = parseAndEvaluate("(load \"lib/prelude.scm\")");
        expectInteger(result, 1, "1", true);
    }
}

TestBase::~TestBase()
{
}

void TestBase::SetUp()
{
}

void TestBase::TearDown()
{
    engine.reset(false);
}

ExprHandle TestBase::evaluate(ExprHandle input)
{
    return engine.eval(input);
}

ExprHandle TestBase::apply(ExprHandle expr, ExprHandle args)
{
    return engine.apply(expr, args);
}

ExprHandle TestBase::parseAndEvaluate(string const & input)
{
    try {
        ReadEvalString helper(&engine, input);
        ExprHandle rv = helper.run();
        return rv;
    }
    catch ( ScamException e ) {
        ExprHandle rv = ExpressionFactory::makeError(e.getMessage());
        return rv;
    }
    catch ( ... ) {
        return ExpressionFactory::makeError("Unknown exception");
    }
}

ExprHandle TestBase::parseAndEvaluateFile(char const * filename)
{
    stringstream s;
    s << "(load \"" << filename << "\")";
    return parseAndEvaluate(s.str());
}

ExprHandle TestBase::readString(char const * input)
{
    try {
        ReadEvalString helper(&engine, input);
        ExprHandle rv = helper.read();
        return rv;
    }
    catch ( ScamException e ) {
        ExprHandle rv = ExpressionFactory::makeError(e.getMessage());
        return rv;
    }
    catch ( ... ) {
        return ExpressionFactory::makeError("Unknown exception");
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

#define DECODER(B)                                      \
        decodeBit(mismatch, exp, SELECT_ ## B, #B, s)

        DECODER(NULL);
        DECODER(ERROR);
        DECODER(TRUTH);

        DECODER(CHAR);
        DECODER(STRING);
        DECODER(SYMBOL);
        DECODER(KEYWORD);

        DECODER(NUMERIC);
        DECODER(COMPLEX);
        DECODER(REAL);
        DECODER(RATIONAL);
        DECODER(BOOLEAN);

        DECODER(NIL);
        DECODER(CONS);
        DECODER(LIST);
        DECODER(VECTOR);

        DECODER(APPLY);
        DECODER(PROC);

        DECODER(CLASS);
        DECODER(INSTANCE);

        DECODER(MANAGED);

#undef DECODER
    }

    return s.str();
}

void TestBase::checkPredicates(ConstExprHandle expr, unsigned exp)
{
    ASSERT_NE(nullptr, expr);
    unsigned act { 0 };

    act |= (expr->isNull() ? SELECT_NULL : 0);
    act |= (expr->error() ? SELECT_ERROR : 0);
    act |= (expr->truth() ? SELECT_TRUTH : 0);

    act |= (expr->isBoolean() ? SELECT_BOOLEAN : 0);
    act |= (expr->isChar() ? SELECT_CHAR : 0);
    act |= (expr->isString() ? SELECT_STRING : 0);
    act |= (expr->isSymbol() ? SELECT_SYMBOL : 0);
    act |= (expr->isKeyword() ? SELECT_KEYWORD : 0);

    act |= (expr->isNumeric() ? SELECT_NUMERIC : 0);
    act |= (expr->isComplex() ? SELECT_COMPLEX : 0);
    act |= (expr->isReal() ? SELECT_REAL : 0);
    act |= (expr->isRational() ? SELECT_RATIONAL : 0);
    act |= (expr->isInteger() ? SELECT_INTEGER : 0);

    act |= (expr->isNil() ? SELECT_NIL : 0);
    act |= (expr->isCons() ? SELECT_CONS : 0);
    act |= (expr->isList() ? SELECT_LIST : 0);
    act |= (expr->isVector() ? SELECT_VECTOR : 0);

    act |= (expr->hasApply() ? SELECT_APPLY : 0);
    act |= (expr->isProcedure() ? SELECT_PROC : 0);

    act |= (expr->isClass() ? SELECT_CLASS : 0);
    act |= (expr->isInstance() ? SELECT_INSTANCE : 0);

    act |= (expr->isDict() ? SELECT_DICT : 0);

    act |= (expr->isManaged() ? SELECT_MANAGED : 0);

    EXPECT_EQ(exp, act) << decodePredicate(act, exp)
                        << "\n\tvalue: " << expr->toString();
}

void expectNonNumeric(ConstExprHandle expr)
{
    EXPECT_FALSE(expr->isNumeric());
}

void TestBase::expectNull(ConstExprHandle expr)
{
    ASSERT_TRUE(expr->isNull()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_NULL);
    EXPECT_EQ("null", expr->toString());

    expectNonNumeric(expr);
}

void TestBase::expectError(ConstExprHandle expr,
                           string const msg,
                           bool managed)
{
    ASSERT_TRUE(expr->error()) << "Actual Value: " << expr->toString();;

    auto pred = SELECT_TRUTH | SELECT_ERROR;
    pred |= (managed ? SELECT_MANAGED : 0x0);
    checkPredicates(expr, pred);

    if ( ! msg.empty() ) {
        EXPECT_EQ(msg, expr->toString());
    }

    expectNonNumeric(expr);
}

void TestBase::expectBoolean(ConstExprHandle expr,
                             bool value,
                             string const & repr)
{
    ASSERT_TRUE(expr->isBoolean()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_BOOLEAN | (value ? SELECT_TRUTH : 0));
    EXPECT_EQ(repr, expr->toString());

    expectNonNumeric(expr);
}

void TestBase::expectTrue(string const & input)
{
    ExprHandle expr = parseAndEvaluate(input);
    expectBoolean(expr, true, "#t");
}

void TestBase::expectFalse(string const & input)
{
    ExprHandle expr = parseAndEvaluate(input);
    expectBoolean(expr, false, "#f");
}

void TestBase::booleanTest(ConstExprHandle expr,
                           bool value,
                           string const & repr)
{
    expectBoolean(expr, value, repr);
    ExprHandle evaled = evaluate(const_cast<ExprHandle>(expr));
    expectBoolean(evaled, value, repr);
}

void TestBase::expectSpecialNumeric(ConstExprHandle expr,
                                    std::string const & repr)
{
    ASSERT_TRUE(expr->isNaN() || expr->isNegInf() || expr->isPosInf())
        << "Actual Value: " << expr->toString();;

    EXPECT_EQ(repr, expr->toString());
    EXPECT_TRUE(expr->isReal());
    EXPECT_FALSE(expr->isRational());
}

void TestBase::expectComplex(ConstExprHandle expr,
                             ConstExprHandle real,
                             ConstExprHandle imag,
                             std::string const & repr,
                             bool exact)
{
    ASSERT_TRUE(expr->isComplex()) << "Actual Value: " << expr->toString();

    checkPredicates(expr, SELECT_TRUTH | ALL_COMPLEX);
    EXPECT_EQ(repr, expr->toString());

    EXPECT_FALSE(expr->isReal());
    EXPECT_FALSE(expr->isRational());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_EQ(exact, expr->isExact());
}

void TestBase::expectReal(ConstExprHandle expr,
                          double value,
                          string const & repr,
                          bool exact)
{
    ASSERT_TRUE(expr->isReal()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_TRUTH | ALL_REAL);
    EXPECT_EQ(repr, expr->toString());

    try {
        EXPECT_FLOAT_EQ(value, expr->asDouble());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }

    EXPECT_FALSE(expr->isRational());
    EXPECT_FALSE(expr->isInteger());
    EXPECT_EQ(exact, expr->isExact());
}

void TestBase::expectRational(ConstExprHandle expr,
                              const std::pair<int, int> & value,
                              std::string const & repr,
                              bool exact)
{
    ASSERT_TRUE(expr->isRational()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_TRUTH | ALL_RATIONAL);
    EXPECT_EQ(repr, expr->toString());
    try {
        EXPECT_EQ(value, expr->asRational());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }

    EXPECT_FALSE(expr->isInteger());
    EXPECT_EQ(exact, expr->isExact());

}

void TestBase::expectInteger(ConstExprHandle expr,
                             int value,
                             string const & repr,
                             bool exact)
{
    ASSERT_TRUE(expr->isInteger()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ(repr, expr->toString());
    try {
        EXPECT_EQ(value, expr->asInteger());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }

    EXPECT_EQ(exact, expr->isExact());
}

void TestBase::expectChar(ConstExprHandle expr,
                          char value,
                          string const & repr)
{
    ASSERT_TRUE(expr->isChar()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_TRUTH | SELECT_CHAR | SELECT_MANAGED);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(value, expr->toChar());
}

void TestBase::expectString(ConstExprHandle expr, string const & value)
{
    ASSERT_TRUE(expr->isString()) << "Actual Value: " << expr->toString();;

    const auto flags = SELECT_TRUTH | SELECT_STRING | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(value, expr->toString());
}

void TestBase::expectSymbol(ConstExprHandle expr, string const & name)
{
    ASSERT_TRUE(expr->isSymbol()) << "Actual Value: " << expr->toString();;

    const auto flags = SELECT_TRUTH | SELECT_SYMBOL | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(name, expr->toString());
}

void TestBase::expectKeyword(ConstExprHandle expr, string const & name)
{
    ASSERT_TRUE(expr->isKeyword()) << "Actual Value: " << expr->toString();;

    const auto flags = SELECT_TRUTH | SELECT_KEYWORD | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(name, expr->toString());
}

void TestBase::expectNil(ConstExprHandle expr)
{
    ASSERT_TRUE(expr->isNil()) << "Actual Value: " << expr->toString();;

    static const string repr { "()" };
    checkPredicates(expr, SELECT_TRUTH | ALL_NIL);
    EXPECT_EQ(repr, expr->toString());
}

void TestBase::expectList(ConstExprHandle expr,
                          string const & repr,
                          size_t len)
{
    ASSERT_TRUE(expr->isList()) << "Actual Value: " << expr->toString();;

    const auto flags =
        SELECT_TRUTH | SELECT_CONS | SELECT_LIST | SELECT_MANAGED;

    checkPredicates(expr, flags);
    EXPECT_EQ(repr, expr->toString());
    try {
        EXPECT_EQ(len, expr->length());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }
}

void TestBase::expectCons(ConstExprHandle expr, string const & repr)
{
    ASSERT_TRUE(expr->isCons()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS | SELECT_MANAGED);
    EXPECT_EQ(repr, expr->toString());
}

void
TestBase::expectApplicable(ConstExprHandle expr, string const & repr)
{
    ASSERT_TRUE(expr->hasApply()) << "Actual Value: " << expr->toString();;

    const auto flags = SELECT_TRUTH | SELECT_APPLY;
    checkPredicates(expr, flags);
    EXPECT_EQ(repr, expr->toString());
}

void TestBase::expectVector(ConstExprHandle expr,
                            string const & repr,
                            size_t len)
{
    ASSERT_TRUE(expr->isVector()) << "Actual Value: " << expr->toString();;

    const auto flags = SELECT_TRUTH | SELECT_VECTOR | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(len, expr->length());
}

void TestBase::expectProcedure(ConstExprHandle expr,
                               string const & repr)
{
    ASSERT_TRUE(expr->isProcedure()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_TRUTH | ALL_PROC);
    EXPECT_EQ(repr, expr->toString());
}

void TestBase::expectClass(ConstExprHandle expr)
{
    ASSERT_TRUE(expr->isClass()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_TRUTH | ALL_CLASS);
    EXPECT_EQ("class", expr->toString());
}

void TestBase::expectInstance(ConstExprHandle expr)
{
    ASSERT_TRUE(expr->isInstance()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_TRUTH | ALL_INSTANCE);
    EXPECT_EQ("instance", expr->toString());
}

void TestBase::expectDict(ConstExprHandle expr,
                          int count,
                          string const & repr)
{
    ASSERT_TRUE(expr->isDict()) << "Actual Value: " << expr->toString();;

    checkPredicates(expr, SELECT_TRUTH | ALL_DICT);
    EXPECT_EQ(repr, expr->toString());

    try {
        EXPECT_EQ(count, expr->length());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }
}
