#include "TestBase.hpp"

#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "WorkQueue.hpp"
#include "expr/SequenceOps.hpp"
#include "input/ScamParser.hpp"
#include "input/StringTokenizer.hpp"
#include "port/ScamPort.hpp"
#include "util/FileUtils.hpp"
#include "util/ReadEvalStream.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"

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
    static const unsigned long SELECT_PAIR       { 1 << 13 };
    static const unsigned long SELECT_LIST       { 1 << 14 };
    static const unsigned long SELECT_VECTOR     { 1 << 15 };
    static const unsigned long SELECT_BYTEVECTOR { 1 << 16 };
    static const unsigned long SELECT_APPLY      { 1 << 17 };
    static const unsigned long SELECT_PROC       { 1 << 18 };
    static const unsigned long SELECT_CLASS      { 1 << 19 };
    static const unsigned long SELECT_INSTANCE   { 1 << 20 };
    static const unsigned long SELECT_KEYWORD    { 1 << 21 };
    static const unsigned long SELECT_DICT       { 1 << 22 };
    static const unsigned long SELECT_PORT       { 1 << 23 };
    static const unsigned long SELECT_EOF        { 1 << 24 };
    static const unsigned long SELECT_SYNTAX     { 1 << 25 };
    static const unsigned long SELECT_ENV        { 1 << 26 };

    static const unsigned long SELECT_MANAGED    { 1 << 30 };

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

TestBase::TestBase()
    : engine(ScamEngine::getEngine())
    , mm(engine.getMemoryManager())
{
    mm.reset();
    engine.reset(true);
    extractor = mm.make<Extractor>();
    engine.setCont(extractor);
    handler = mm.make<TestHandler>(extractor);
    engine.pushHandler(handler);
}

TestBase::~TestBase()
{
    engine.popHandler();
}

void TestBase::SetUp()
{
}

void TestBase::TearDown()
{
    engine.reset(false);
}

ScamValue TestBase::evaluate(ScamValue input)
{
    ScamValue rv = engine.eval(input);
    if ( isNothing(rv) ) {
        rv = extractor->getLastValue();
    }
    return rv;
}

ScamValue TestBase::apply(ScamValue expr, ScamValue args)
{
    ScamValue rv = engine.apply(expr, args);
    if ( isNothing(rv) ) {
        rv = extractor->getLastValue();
    }
    return rv;
}

ScamValue TestBase::readString(char const * input)
{
    try {
        StringCharStream stream(input);
        ReadEvalStream   helper(stream);
        ScamValue rv = helper.read();
        return rv;
    }
    catch ( ScamException e ) {
        ScamValue rv = makeError(e.getMessage().c_str(), makeString(input));
        return rv;
    }
    catch ( ... ) {
        return makeError("Unknown exception", makeString(input));
    }
}

ScamValue TestBase::readEval(std::string const & input)
{
    try {
        StringCharStream stream(input);
        ReadEvalStream helper(stream);
        ScamValue rv = helper.run();
        if ( isNothing(rv) ) {
            rv = extractor->getLastValue();
        }
        return rv;
    }
    catch ( ScamException e ) {
        ScamValue rv = makeError(e.getMessage().c_str(), makeString(input));
        return rv;
    }
    catch ( std::exception & e ) {
        ScamValue rv = makeError(e.what(), makeString(input));
        return rv;
    }
    catch ( ... ) {
        return makeError("Unknown exception", makeString(input));
    }
}

ScamValue TestBase::readEvalFile(char const * filename)
{
    string fullpath = findFileOnPath(filename);
    if ( fullpath.empty() ) {
        return makeBoolean(false);
    }

    return loadEvalFile(fullpath);
}

void TestBase::runsafe(std::function<void()> thunk)
{
    ScamValue rv = makeNothing();
    try {
        thunk();
        rv = makeSymbol("ok");
    }
    catch ( ScamException e ) {
        rv = makeError(e.getMessage().c_str());
    }
    catch ( std::exception & e ) {
        rv = makeError(e.what());
    }
    catch ( ... ) {
        rv = makeError("Unknown exception");
    }

    cerr << writeValue(rv) << "\n";

    EXPECT_TRUE(isSymbol(rv));
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
        DECODER(PAIR);
        DECODER(LIST);
        DECODER(VECTOR);
        DECODER(BYTEVECTOR);

        DECODER(APPLY);
        DECODER(PROC);

        DECODER(CLASS);
        DECODER(INSTANCE);

        DECODER(DICT);
        DECODER(PORT);
        DECODER(EOF);

        DECODER(MANAGED);

#undef DECODER
    }

    return s.str();
}

void TestBase::checkPredicates(ScamValue expr, unsigned exp)
{
    ASSERT_NE(nullptr, expr);
    unsigned act { 0 };

    act |= (isNothing(expr) ? SELECT_NULL : 0);
    act |= (isError(expr) ? SELECT_ERROR : 0);
    act |= (truth(expr) ? SELECT_TRUTH : 0);

    act |= (isBoolean(expr) ? SELECT_BOOLEAN : 0);
    act |= (isChar(expr) ? SELECT_CHAR : 0);
    act |= (isString(expr) ? SELECT_STRING : 0);
    act |= (isSymbol(expr) ? SELECT_SYMBOL : 0);
    act |= (isKeyword(expr) ? SELECT_KEYWORD : 0);

    act |= (isNumeric(expr) ? SELECT_NUMERIC : 0);
    act |= (isComplex(expr) ? SELECT_COMPLEX : 0);
    act |= (isReal(expr) ? SELECT_REAL : 0);
    act |= (isRational(expr) ? SELECT_RATIONAL : 0);
    act |= (isInteger(expr) ? SELECT_INTEGER : 0);

    act |= (isNull(expr) ? SELECT_NIL : 0);
    act |= (isPair(expr) ? SELECT_PAIR : 0);
    act |= (isList(expr) ? SELECT_LIST : 0);
    act |= (isVector(expr) ? SELECT_VECTOR : 0);
    act |= (isByteVector(expr) ? SELECT_BYTEVECTOR : 0);

    act |= (isApplicable(expr) ? SELECT_APPLY : 0);
    act |= (isProcedure(expr) ? SELECT_PROC : 0);

    act |= (isClass(expr) ? SELECT_CLASS : 0);
    act |= (isInstance(expr) ? SELECT_INSTANCE : 0);

    act |= (isDict(expr) ? SELECT_DICT : 0);
    act |= (isPort(expr) ? SELECT_PORT : 0);
    act |= (isEof(expr) ? SELECT_EOF : 0);
    act |= (isSyntax(expr) ? SELECT_SYNTAX : 0);
    act |= (isEnv(expr) ? SELECT_ENV : 0);

    act |= (expr->isManaged() ? SELECT_MANAGED : 0);

    EXPECT_EQ(exp, act) << decodePredicate(act, exp)
                        << "\n\tvalue: " << writeValue(expr);
}

void TestBase::assertType(ScamValue value,
                          const char * name,
                          function<bool(ScamValue)> pred)
{
    ASSERT_TRUE(pred(value))
        << "Expected type " << name << "; got " << writeValue(value);
}


void expectNonNumeric(ScamValue expr)
{
    EXPECT_FALSE(isNumeric(expr));
}

void TestBase::expectNothing(ScamValue expr)
{
    assertType(expr, "null", isNothing);
    checkPredicates(expr, SELECT_NULL);
    EXPECT_EQ("null", writeValue(expr));

    expectNonNumeric(expr);
}

void TestBase::expectError(ScamValue expr, string const msg)
{
    assertType(expr, "error", isError);
    auto pred = SELECT_TRUTH | SELECT_ERROR | SELECT_MANAGED;
    checkPredicates(expr, pred);

    if ( ! msg.empty() ) {
        EXPECT_EQ(msg, writeValue(expr));
    }

    expectNonNumeric(expr);
}

void TestBase::expectBoolean(ScamValue expr,
                             bool value,
                             string const & repr)
{
    assertType(expr, "boolean", isBoolean);
    checkPredicates(expr, SELECT_BOOLEAN | (value ? SELECT_TRUTH : 0));
    EXPECT_EQ(repr, writeValue(expr));

    expectNonNumeric(expr);
}

void TestBase::expectTrue(string const & input)
{
    ScamValue expr = readEval(input);
    expectBoolean(expr, true, "#t");
}

void TestBase::expectFalse(string const & input)
{
    ScamValue expr = readEval(input);
    expectBoolean(expr, false, "#f");
}

void TestBase::booleanTest(ScamValue expr,
                           bool value,
                           string const & repr)
{
    expectBoolean(expr, value, repr);
    ScamValue evaled = evaluate(const_cast<ScamValue>(expr));
    expectBoolean(evaled, value, repr);
}

void TestBase::expectSpecialNumeric(ScamValue expr,
                                    std::string const & repr)
{
    assertType(expr, "special numeric", isSpecialNumeric);
    EXPECT_EQ(repr, writeValue(expr));
    EXPECT_TRUE(isReal(expr));
    EXPECT_FALSE(isRational(expr));
}

void TestBase::expectComplex(ScamValue expr,
                             ScamValue real,
                             ScamValue imag,
                             std::string const & repr,
                             bool exact)
{
    assertType(expr, "complex", isComplex);
    checkPredicates(expr, SELECT_TRUTH | ALL_COMPLEX);
    EXPECT_EQ(repr, writeValue(expr));

    EXPECT_FALSE(isReal(expr));
    EXPECT_FALSE(isRational(expr));
    EXPECT_FALSE(isInteger(expr));
    EXPECT_EQ(exact, isExact(expr));
}

void TestBase::expectReal(ScamValue expr,
                          double value,
                          string const & repr,
                          bool exact)
{
    assertType(expr, "real", isReal);
    checkPredicates(expr, SELECT_TRUTH | ALL_REAL);
    EXPECT_EQ(repr, writeValue(expr));

    EXPECT_FLOAT_EQ(value, asDouble(expr));
    EXPECT_FALSE(isRational(expr));
    EXPECT_FALSE(isInteger(expr));
    EXPECT_EQ(exact, isExact(expr));
}

void TestBase::expectRational(ScamValue expr,
                              const RationalPair & value,
                              std::string const & repr,
                              bool exact)
{
    assertType(expr, "rational", isRational);
    checkPredicates(expr, SELECT_TRUTH | ALL_RATIONAL);
    EXPECT_EQ(repr, writeValue(expr));

    const auto act = asRational(expr);
    EXPECT_EQ(value.num, act.num);
    EXPECT_EQ(value.den, act.den);

    EXPECT_FALSE(isInteger(expr));
    EXPECT_EQ(exact, isExact(expr));

}

void TestBase::expectIntegerCommon(ScamValue expr,
                                   string const & repr,
                                   bool exact)
{
    assertType(expr, "integer", isInteger);
    checkPredicates(expr, SELECT_TRUTH | ALL_INTEGER);
    EXPECT_EQ(repr, writeValue(expr));
    EXPECT_EQ(exact, isExact(expr));
}

void TestBase::expectInteger(ScamValue expr,
                             int value,
                             string const & repr,
                             bool exact)
{
    expectIntegerCommon(expr, repr, exact);
    EXPECT_EQ(value, asInteger(expr));
}

void TestBase::expectInteger(ScamValue expr,
                             mpz_t & value,
                             string const & repr,
                             bool exact)
{
    expectIntegerCommon(expr, repr, exact);
    EXPECT_EQ(0, mpz_cmp(value, expr->intPart()));
}

void TestBase::expectChar(ScamValue expr,
                          char value,
                          string const & repr)
{
    assertType(expr, "character", isChar);
    checkPredicates(expr, SELECT_TRUTH | SELECT_CHAR | SELECT_MANAGED);
    EXPECT_EQ(repr, writeValue(expr));
    EXPECT_EQ(value, asChar(expr));
}

void TestBase::expectString(ScamValue expr, string const & value)
{
    assertType(expr, "string", isString);
    const auto flags = SELECT_TRUTH | SELECT_STRING | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(value, writeValue(expr));
}

void TestBase::expectSymbol(ScamValue expr, string const & name)
{
    assertType(expr, "symbol", isSymbol);
    const auto flags = SELECT_TRUTH | SELECT_SYMBOL | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(name, writeValue(expr));
}

void TestBase::expectKeyword(ScamValue expr, string const & name)
{
    assertType(expr, "keyword", isKeyword);
    const auto flags = SELECT_TRUTH | SELECT_KEYWORD | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(name, writeValue(expr));
}

void TestBase::expectNull(ScamValue expr)
{
    assertType(expr, "nil", isNull);
    static const string repr { "()" };
    checkPredicates(expr, SELECT_TRUTH | ALL_NIL);
    EXPECT_EQ(repr, writeValue(expr));
}

void TestBase::expectList(ScamValue expr, string const & repr, size_t len)
{
    assertType(expr, "list", isList);
    const auto flags =
        SELECT_TRUTH | SELECT_PAIR | SELECT_LIST | SELECT_MANAGED;

    checkPredicates(expr, flags);
    EXPECT_EQ(repr, writeValue(expr));
    ScamValue hack = const_cast<ScamValue>(expr);
    EXPECT_EQ(len, length(hack));
}

void TestBase::expectPair(ScamValue expr, string const & repr)
{
    assertType(expr, "pair", isPair);
    checkPredicates(expr, SELECT_TRUTH | SELECT_PAIR | SELECT_MANAGED);
    EXPECT_EQ(repr, writeValue(expr));
}

void
TestBase::expectApplicable(ScamValue expr, string const & repr, bool managed)
{
    ASSERT_TRUE(isApplicable(expr)) << "Actual Value: " << writeValue(expr);

    const auto flags = SELECT_TRUTH | SELECT_APPLY | (managed ? SELECT_MANAGED : 0);
    checkPredicates(expr, flags);
    EXPECT_EQ(repr, writeValue(expr));
}

void TestBase::expectVector(ScamValue expr,
                            string const & repr,
                            size_t len)
{
    assertType(expr, "vector", isVector);
    const auto flags = SELECT_TRUTH | SELECT_VECTOR | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(repr, writeValue(expr));
    ScamValue hack = const_cast<ScamValue>(expr);
    EXPECT_EQ(len, length(hack));
}

void TestBase::expectByteVector(ScamValue expr,
                                string const & repr,
                                size_t len)
{
    assertType(expr, "byte vector", isByteVector);
    const auto flags = SELECT_TRUTH | SELECT_BYTEVECTOR | SELECT_MANAGED;
    checkPredicates(expr, flags);
    EXPECT_EQ(repr, writeValue(expr));
    ScamValue hack = const_cast<ScamValue>(expr);
    EXPECT_EQ(len, length(hack));
}

void TestBase::expectProcedure(ScamValue expr, string const & repr)
{
    assertType(expr, "procedure", isProcedure);
    checkPredicates(expr, SELECT_TRUTH | ALL_PROC);
    EXPECT_EQ(repr, writeValue(expr));
}

void TestBase::expectClass(ScamValue expr)
{
    assertType(expr, "class", isClass);
    checkPredicates(expr, SELECT_TRUTH | ALL_CLASS);
    EXPECT_EQ("class", writeValue(expr));
}

void TestBase::expectInstance(ScamValue expr)
{
    assertType(expr, "instance", isInstance);
    checkPredicates(expr, SELECT_TRUTH | ALL_INSTANCE);
    EXPECT_EQ("instance", writeValue(expr));
}

void TestBase::expectDict(ScamValue expr, int count, string const & repr)
{
    assertType(expr, "dictionary", isDict);
    checkPredicates(expr, SELECT_TRUTH | ALL_DICT);
    EXPECT_EQ(repr, writeValue(expr));
    EXPECT_EQ(count, length(expr));
}

void TestBase::expectPort(ScamValue expr,
                          const std::string & repr,
                          const std::string & contents)
{
    assertType(expr, "port", isPort);
    checkPredicates(expr, SELECT_TRUTH | SELECT_PORT | SELECT_MANAGED);

    EXPECT_EQ(repr, writeValue(expr));

    ScamPort * port = asPort(expr);
    ASSERT_NE(nullptr, port);

    ScamValue test = port->getContents();
    expectString(test, contents);
}

void TestBase::expectEof(ScamValue expr)
{
    assertType(expr, "eof", isEof);
    checkPredicates(expr, SELECT_TRUTH | SELECT_EOF);

    EXPECT_EQ(string("eof"), writeValue(expr));
}

void TestBase::expectSyntax(ScamValue expr, const std::string & repr)
{
    assertType(expr, "syntax", isSyntax);
    checkPredicates(expr, SELECT_TRUTH | SELECT_SYNTAX | SELECT_MANAGED);

    EXPECT_EQ(repr, writeValue(expr));
}

void TestBase::expectEnv(ScamValue expr)
{
    assertType(expr, "env", isEnv);
    checkPredicates(expr, SELECT_TRUTH | SELECT_ENV | SELECT_MANAGED);
}
