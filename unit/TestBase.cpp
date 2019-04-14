#include "TestBase.hpp"

#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"
#include "util/EvalString.hpp"

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
    static const unsigned long SELECT_FLOAT    { 1 << 7 };
    static const unsigned long SELECT_INTEGER  { 1 << 8 };
    static const unsigned long SELECT_BOOLEAN  { 1 << 9 };
    static const unsigned long SELECT_NIL      { 1 << 10 };
    static const unsigned long SELECT_CONS     { 1 << 11 };
    static const unsigned long SELECT_LIST     { 1 << 12 };
    static const unsigned long SELECT_VECTOR   { 1 << 13 };
    static const unsigned long SELECT_APPLY    { 1 << 14 };
    static const unsigned long SELECT_PROC     { 1 << 15 };
    static const unsigned long SELECT_CLASS    { 1 << 16 };
    static const unsigned long SELECT_INSTANCE { 1 << 17 };
    static const unsigned long SELECT_KEYWORD  { 1 << 18 };
    static const unsigned long SELECT_DICT     { 1 << 19 };
    static const unsigned long SELECT_MANAGED  { 1 << 20 };

    static const unsigned long ALL_FLOAT      = SELECT_NUMERIC | SELECT_FLOAT | SELECT_MANAGED;
    static const unsigned long ALL_INTEGER    = ALL_FLOAT | SELECT_INTEGER;
    static const unsigned long ALL_NIL        = SELECT_NIL | SELECT_LIST;
    static const unsigned long ALL_PROC       = SELECT_APPLY | SELECT_PROC | SELECT_MANAGED;
    static const unsigned long ALL_CLASS      = SELECT_CLASS | ALL_PROC;
    static const unsigned long ALL_INSTANCE   = SELECT_INSTANCE | ALL_PROC;
    static const unsigned long ALL_DICT       = SELECT_DICT | SELECT_APPLY | SELECT_MANAGED;
}

TestBase::TestBase()
    : mm(standardMemoryManager)
{
}

TestBase::~TestBase()
{
}

void TestBase::SetUp()
{
    mm.reset();
    engine.reset(true);
    extractor = mm.make<Extractor>();
    engine.setCont(extractor);
    ScamExpr * result = parseAndEvaluate("(load \"lib/prelude.scm\")");
    expectInteger(result, 1, "1");
}

void TestBase::TearDown()
{
    engine.reset(false);
}

ScamExpr * TestBase::evaluate(ScamExpr * input)
{
    return engine.eval(input);
}

ScamExpr * TestBase::apply(ScamExpr * expr, ScamExpr * args)
{
    return engine.apply(expr, args);
}

ScamExpr * TestBase::parseAndEvaluate(string const & input)
{
    try {
        EvalString helper(&engine, input);
        ScamExpr * rv = helper.run();
        return rv;
    }
    catch ( ScamException e ) {
        ScamExpr * rv = ExpressionFactory::makeError(e.getMessage());
        return rv;
    }
    catch ( ... ) {
        return ExpressionFactory::makeError("Unknown exception");
    }
}

ScamExpr * TestBase::parseAndEvaluateFile(char const * filename)
{
    stringstream s;
    s << "(load \"" << filename << "\")";
    return parseAndEvaluate(s.str());
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
        DECODER(KEYWORD);

        DECODER(NUMERIC);
        DECODER(FLOAT);
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

void TestBase::checkPredicates(ScamExpr * expr, unsigned exp)
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
    act |= (expr->isFloat() ? SELECT_FLOAT : 0);
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

void expectNonNumeric(ScamExpr * expr)
{
    EXPECT_THROW(expr->toFloat(), ScamException) << "got " << expr->toFloat();
    EXPECT_THROW(expr->toInteger(), ScamException) << "got "  << expr->toInteger();
}

void TestBase::expectNull(ScamExpr * expr)
{
    checkPredicates(expr, SELECT_NULL);
    EXPECT_EQ("null", expr->toString());

    expectNonNumeric(expr);
}

void TestBase::expectError(ScamExpr * expr,
                                     string const msg,
                                     bool managed)
{
    auto pred = SELECT_TRUTH | SELECT_ERROR | (managed ? SELECT_MANAGED : 0x0);
    checkPredicates(expr, pred);

    if ( ! msg.empty() ) {
        EXPECT_EQ(msg, expr->toString());
    }

    expectNonNumeric(expr);
}

void TestBase::expectBoolean(ScamExpr * expr,
                                       bool value,
                                       string const & repr)
{
    checkPredicates(expr, SELECT_BOOLEAN | (value ? SELECT_TRUTH : 0));
    EXPECT_EQ(repr, expr->toString());

    expectNonNumeric(expr);
}

void TestBase::expectTrue(string const & input)
{
    ScamExpr * expr = parseAndEvaluate(input);
    expectBoolean(expr, true, "#t");
}

void TestBase::expectFalse(string const & input)
{
    ScamExpr * expr = parseAndEvaluate(input);
    expectBoolean(expr, false, "#f");
}

void TestBase::booleanTest(ScamExpr * expr,
                                     bool value,
                                     string const & repr)
{
    expectBoolean(expr, value, repr);
    ScamExpr * evaled = evaluate(expr);
    expectBoolean(evaled, value, repr);
}

void TestBase::expectFloat(ScamExpr * expr,
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

void TestBase::expectInteger(ScamExpr * expr,
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
TestBase::expectChar(ScamExpr * expr, char value, string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_CHAR | SELECT_MANAGED);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(value, expr->toChar());
}


void TestBase::expectString(ScamExpr * expr, string const & value)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_STRING | SELECT_MANAGED);
    EXPECT_EQ(value, expr->toString());
}

void TestBase::expectSymbol(ScamExpr * expr, string const & name)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_SYMBOL | SELECT_MANAGED);
    EXPECT_EQ(name, expr->toString());
}

void TestBase::expectKeyword(ScamExpr * expr, string const & name)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_KEYWORD | SELECT_MANAGED);
    EXPECT_EQ(name, expr->toString());
}

void TestBase::expectNil(ScamExpr * expr)
{
    static const string repr { "()" };
    checkPredicates(expr, SELECT_TRUTH | ALL_NIL);
    EXPECT_EQ(repr, expr->toString());
}

void TestBase::expectList(ScamExpr * expr,
                                    string const & repr,
                                    size_t len)
{
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

void TestBase::expectCons(ScamExpr * expr, string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_CONS | SELECT_MANAGED);
    EXPECT_EQ(repr, expr->toString());
}

void TestBase::expectApplicable(ScamExpr * expr, string const & repr)
{
    const auto flags = SELECT_TRUTH | SELECT_APPLY;
    checkPredicates(expr, flags);
    EXPECT_EQ(repr, expr->toString());
}

void TestBase::expectVector(ScamExpr * expr,
                                      string const & repr,
                                      size_t len)
{
    checkPredicates(expr, SELECT_TRUTH | SELECT_VECTOR | SELECT_MANAGED);
    EXPECT_EQ(repr, expr->toString());
    EXPECT_EQ(len, expr->length());
}

void TestBase::expectProcedure(ScamExpr * expr, string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_PROC);
    EXPECT_EQ(repr, expr->toString());
}

void TestBase::expectClass(ScamExpr * expr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_CLASS);
    EXPECT_EQ("class", expr->toString());
}

void TestBase::expectInstance(ScamExpr * expr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_INSTANCE);
    EXPECT_EQ("instance", expr->toString());
}

void
TestBase::expectDict(ScamExpr * expr, int count, string const & repr)
{
    checkPredicates(expr, SELECT_TRUTH | ALL_DICT);
    EXPECT_EQ(repr, expr->toString());

    try {
        EXPECT_EQ(count, expr->length());
    }
    catch ( ScamException e ) {
        FAIL() << e.getMessage() << "\n";
    }
}
