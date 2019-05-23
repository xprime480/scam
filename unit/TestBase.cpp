#include "TestBase.hpp"

#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"
#include "util/ReadEvalString.hpp"

using namespace scam;
using namespace std;

namespace
{
    static const unsigned long SELECT_NULL       { 1 << 0 };
    static const unsigned long SELECT_ERROR      { 1 << 1 };
    static const unsigned long SELECT_TRUTH      { 1 << 2 };
    static const unsigned long SELECT_CHAR       { 1 << 3 };
    static const unsigned long SELECT_STRING     { 1 << 4 };
    static const unsigned long SELECT_SYMBOL     { 1 << 5 };
    static const unsigned long SELECT_NUMERIC    { 1 << 6 };
    static const unsigned long SELECT_COMPLEX    { 1 << 7 };
    static const unsigned long SELECT_REAL       { 1 << 8 };
    static const unsigned long SELECT_RATIONAL   { 1 << 9 };
    static const unsigned long SELECT_INTEGER    { 1 << 10 };
    static const unsigned long SELECT_BOOLEAN    { 1 << 11 };
    static const unsigned long SELECT_NIL        { 1 << 12 };
    static const unsigned long SELECT_CONS       { 1 << 13 };
    static const unsigned long SELECT_LIST       { 1 << 14 };
    static const unsigned long SELECT_VECTOR     { 1 << 15 };
    static const unsigned long SELECT_BYTEVECTOR { 1 << 16 };
    static const unsigned long SELECT_APPLY      { 1 << 17 };
    static const unsigned long SELECT_PROC       { 1 << 18 };
    static const unsigned long SELECT_CLASS      { 1 << 19 };
    static const unsigned long SELECT_INSTANCE   { 1 << 20 };
    static const unsigned long SELECT_KEYWORD    { 1 << 21 };
    static const unsigned long SELECT_DICT       { 1 << 22 };
    static const unsigned long SELECT_MANAGED    { 1 << 23 };

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
        DECODER(BYTEVECTOR);

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

    act |= (TypePredicates::isNull(expr) ? SELECT_NULL : 0);
    act |= (TypePredicates::error(expr) ? SELECT_ERROR : 0);
    act |= (TypePredicates::truth(expr) ? SELECT_TRUTH : 0);

    act |= (TypePredicates::isBoolean(expr) ? SELECT_BOOLEAN : 0);
    act |= (TypePredicates::isChar(expr) ? SELECT_CHAR : 0);
    act |= (TypePredicates::isString(expr) ? SELECT_STRING : 0);
    act |= (TypePredicates::isSymbol(expr) ? SELECT_SYMBOL : 0);
    act |= (TypePredicates::isKeyword(expr) ? SELECT_KEYWORD : 0);

    act |= (TypePredicates::isNumeric(expr) ? SELECT_NUMERIC : 0);
    act |= (TypePredicates::isComplex(expr) ? SELECT_COMPLEX : 0);
    act |= (TypePredicates::isReal(expr) ? SELECT_REAL : 0);
    act |= (TypePredicates::isRational(expr) ? SELECT_RATIONAL : 0);
    act |= (TypePredicates::isInteger(expr) ? SELECT_INTEGER : 0);

    act |= (TypePredicates::isNil(expr) ? SELECT_NIL : 0);
    act |= (TypePredicates::isCons(expr) ? SELECT_CONS : 0);
    act |= (TypePredicates::isList(expr) ? SELECT_LIST : 0);
    act |= (TypePredicates::isVector(expr) ? SELECT_VECTOR : 0);
    act |= (TypePredicates::isByteVector(expr) ? SELECT_BYTEVECTOR : 0);

    act |= (expr->hasApply() ? SELECT_APPLY : 0);
    act |= (TypePredicates::isProcedure(expr) ? SELECT_PROC : 0);

    act |= (TypePredicates::isClass(expr) ? SELECT_CLASS : 0);
    act |= (TypePredicates::isInstance(expr) ? SELECT_INSTANCE : 0);

    act |= (TypePredicates::isDict(expr) ? SELECT_DICT : 0);

    act |= (expr->isManaged() ? SELECT_MANAGED : 0);

    EXPECT_EQ(exp, act) << decodePredicate(act, exp)
                        << "\n\tvalue: " << ExprWriter::write(expr);
}

void expectNonNumeric(ConstExprHandle expr)
{
    EXPECT_FALSE(TypePredicates::isNumeric(expr));
}

void TestBase::expectNull(ConstExprHandle expr)
{
    ASSERT_TRUE(TypePredicates::isNull(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_NULL);
    EXPECT_EQ("null", ExprWriter::write(expr));

    expectNonNumeric(expr);
}

void TestBase::expectError(ConstExprHandle expr,
                           string const msg,
                           bool managed)
{
    ASSERT_TRUE(TypePredicates::error(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    auto pred = SELECT_TRUTH | SELECT_ERROR;
    pred |= (managed ? SELECT_MANAGED : 0x0);
    checkPredicates(expr, pred);

    if ( ! msg.empty() ) {
        EXPECT_EQ(msg, ExprWriter::write(expr));
    }

    expectNonNumeric(expr);
}

void TestBase::expectBoolean(ConstExprHandle expr,
                             bool value,
                             string const & repr)
{
    ASSERT_TRUE(TypePredicates::isBoolean(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_BOOLEAN | (value ? SELECT_TRUTH : 0));
    EXPECT_EQ(repr, ExprWriter::write(expr));

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
    ASSERT_TRUE(TypePredicates::isNaN(expr) ||
                TypePredicates::isNegInf(expr) ||
                TypePredicates::isPosInf(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    EXPECT_EQ(repr, ExprWriter::write(expr));
    EXPECT_TRUE(TypePredicates::isReal(expr));
    EXPECT_FALSE(TypePredicates::isRational(expr));
}

void TestBase::expectComplex(ConstExprHandle expr,
                             ConstExprHandle real,
                             ConstExprHandle imag,
                             std::string const & repr,
                             bool exact)
{
    ASSERT_TRUE(TypePredicates::isComplex(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | ALL_COMPLEX);
    EXPECT_EQ(repr, ExprWriter::write(expr));

    EXPECT_FALSE(TypePredicates::isReal(expr));
    EXPECT_FALSE(TypePredicates::isRational(expr));
    EXPECT_FALSE(TypePredicates::isInteger(expr));
    EXPECT_EQ(exact, TypePredicates::isExact(expr));
}

void TestBase::expectReal(ConstExprHandle expr,
                          double value,
                          string const & repr,
                          bool exact)
{
    ASSERT_TRUE(TypePredicates::isReal(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | ALL_REAL);
    EXPECT_EQ(repr, ExprWriter::write(expr));

    try {
        EXPECT_FLOAT_EQ(value, expr->asDouble());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }

    EXPECT_FALSE(TypePredicates::isRational(expr));
    EXPECT_FALSE(TypePredicates::isInteger(expr));
    EXPECT_EQ(exact, TypePredicates::isExact(expr));
}

void TestBase::expectRational(ConstExprHandle expr,
                              const std::pair<int, int> & value,
                              std::string const & repr,
                              bool exact)
{
    ASSERT_TRUE(TypePredicates::isRational(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | ALL_RATIONAL);
    EXPECT_EQ(repr, ExprWriter::write(expr));
    try {
        EXPECT_EQ(value, expr->asRational());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }

    EXPECT_FALSE(TypePredicates::isInteger(expr));
    EXPECT_EQ(exact, TypePredicates::isExact(expr));

}

void TestBase::expectInteger(ConstExprHandle expr,
                             int value,
                             string const & repr,
                             bool exact)
{
    ASSERT_TRUE(TypePredicates::isInteger(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ(repr, ExprWriter::write(expr));
    try {
        EXPECT_EQ(value, expr->asInteger());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }

    EXPECT_EQ(exact, TypePredicates::isExact(expr));
}

void TestBase::expectChar(ConstExprHandle expr,
                          char value,
                          string const & repr)
{
    ASSERT_TRUE(TypePredicates::isChar(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | SELECT_CHAR | SELECT_MANAGED);
    EXPECT_EQ(repr, ExprWriter::write(expr));
    EXPECT_EQ(value, expr->toChar());
}

void TestBase::expectString(ConstExprHandle expr, string const & value)
{
    ASSERT_TRUE(TypePredicates::isString(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    const auto flags = SELECT_TRUTH | SELECT_STRING | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(value, ExprWriter::write(expr));
}

void TestBase::expectSymbol(ConstExprHandle expr, string const & name)
{
    ASSERT_TRUE(TypePredicates::isSymbol(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    const auto flags = SELECT_TRUTH | SELECT_SYMBOL | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(name, ExprWriter::write(expr));
}

void TestBase::expectKeyword(ConstExprHandle expr, string const & name)
{
    ASSERT_TRUE(TypePredicates::isKeyword(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    const auto flags = SELECT_TRUTH | SELECT_KEYWORD | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(name, ExprWriter::write(expr));
}

void TestBase::expectNil(ConstExprHandle expr)
{
    ASSERT_TRUE(TypePredicates::isNil(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    static const string repr { "()" };
    checkPredicates(expr, SELECT_TRUTH | ALL_NIL);
    EXPECT_EQ(repr, ExprWriter::write(expr));
}

void TestBase::expectList(ConstExprHandle expr,
                          string const & repr,
                          size_t len)
{
    ASSERT_TRUE(TypePredicates::isList(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    const auto flags =
        SELECT_TRUTH | SELECT_CONS | SELECT_LIST | SELECT_MANAGED;

    checkPredicates(expr, flags);
    EXPECT_EQ(repr, ExprWriter::write(expr));
    try {
        EXPECT_EQ(len, expr->length());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }
}

void TestBase::expectCons(ConstExprHandle expr, string const & repr)
{
    ASSERT_TRUE(TypePredicates::isCons(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS | SELECT_MANAGED);
    EXPECT_EQ(repr, ExprWriter::write(expr));
}

void
TestBase::expectApplicable(ConstExprHandle expr, string const & repr)
{
    ASSERT_TRUE(expr->hasApply())
        << "Actual Value: " << ExprWriter::write(expr);

    const auto flags = SELECT_TRUTH | SELECT_APPLY;
    checkPredicates(expr, flags);
    EXPECT_EQ(repr, ExprWriter::write(expr));
}

void TestBase::expectVector(ConstExprHandle expr,
                            string const & repr,
                            size_t len)
{
    ASSERT_TRUE(TypePredicates::isVector(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    const auto flags = SELECT_TRUTH | SELECT_VECTOR | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(repr, ExprWriter::write(expr));
    EXPECT_EQ(len, expr->length());
}

void TestBase::expectByteVector(ConstExprHandle expr,
                                string const & repr,
                                size_t len)
{
    ASSERT_TRUE(TypePredicates::isByteVector(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    const auto flags = SELECT_TRUTH | SELECT_BYTEVECTOR | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(repr, ExprWriter::write(expr));
    EXPECT_EQ(len, expr->length());
}

void TestBase::expectProcedure(ConstExprHandle expr,
                               string const & repr)
{
    ASSERT_TRUE(TypePredicates::isProcedure(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | ALL_PROC);
    EXPECT_EQ(repr, ExprWriter::write(expr));
}

void TestBase::expectClass(ConstExprHandle expr)
{
    ASSERT_TRUE(TypePredicates::isClass(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | ALL_CLASS);
    EXPECT_EQ("class", ExprWriter::write(expr));
}

void TestBase::expectInstance(ConstExprHandle expr)
{
    ASSERT_TRUE(TypePredicates::isInstance(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | ALL_INSTANCE);
    EXPECT_EQ("instance", ExprWriter::write(expr));
}

void TestBase::expectDict(ConstExprHandle expr,
                          int count,
                          string const & repr)
{
    ASSERT_TRUE(TypePredicates::isDict(expr))
        << "Actual Value: " << ExprWriter::write(expr);

    checkPredicates(expr, SELECT_TRUTH | ALL_DICT);
    EXPECT_EQ(repr, ExprWriter::write(expr));

    try {
        EXPECT_EQ(count, expr->length());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }
}
