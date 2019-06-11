#include "TestBase.hpp"
#include "StaticTokenizer.hpp"

#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ScamParser.hpp"

#include <vector>

using namespace scam;
using namespace scam::test_impl;
using namespace std;

class ParserTest : public TestBase
{
protected:
    ParserTest()
        : TestBase(false)
    {
    }

    ScamValue runTest(vector<Token> const & tokens)
    {
        StaticTokenizer tokenizer(tokens);
        ScamParser parser(tokenizer);
        ScamValue expr = parser.parseExpr();
        EXPECT_NE(nullptr, expr);
        return expr;
    }

    void booltest(string const & msg, bool value)
    {
        vector<Token> tokens {
            Token(TokenType::TT_BOOLEAN, msg)
        };

        ScamValue expr = runTest(tokens);
        expectBoolean(expr, value, msg);
    }
};

TEST_F(ParserTest, NoneToken)
{
    vector<Token> tokens {
        Token(TokenType::TT_NONE, "")
    };
    ScamValue expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, EndOfInput)
{
    vector<Token> tokens {
    };

    ScamValue expr = runTest(tokens);
    expectNull(expr);
}

TEST_F(ParserTest, ScanError)
{
    string const msg{ "blah" };
    vector<Token> tokens {
        Token(TokenType::TT_SCAN_ERROR, msg)
    };

    ScamValue expr = runTest(tokens);
    expectError(expr, msg);
}

TEST_F(ParserTest, BoolTrue)
{
    booltest("#t", true);
}

TEST_F(ParserTest, BoolFalse)
{
    booltest("#f", false);
}

TEST_F(ParserTest, CharacterTest)
{
    static const string msg{ "#\\Z" };
    vector<Token> tokens {
        Token(TokenType::TT_CHARACTER, "Z")
    };

    ScamValue expr = runTest(tokens);
    expectChar(expr, 'Z', msg);
}

TEST_F(ParserTest, StringTest)
{
    static const string msg{ "Holy Test Coverage, Batman!" };
    static const string repr{ "\"Holy Test Coverage, Batman!\"" };
    vector<Token> tokens {
        Token(TokenType::TT_STRING, msg)
    };

    ScamValue expr = runTest(tokens);
    expectString(expr, repr);
}

TEST_F(ParserTest, SymbolTest)
{
    static const string msg{ "nil?" };
    vector<Token> tokens {
        Token(TokenType::TT_SYMBOL, msg)
    };

    ScamValue expr = runTest(tokens);
    expectSymbol(expr, msg);
}

TEST_F(ParserTest, RealTest)
{
    static const string msg{ "-17.5" };
    vector<Token> tokens {
        Token(TokenType::TT_NUMERIC, msg, makeReal(-17.5, false))
    };

    ScamValue expr = runTest(tokens);
    expectReal(expr, -17.5, msg, false);
}

TEST_F(ParserTest, IntegerTest)
{
    static const string msg{ "99" };
    vector<Token> tokens {
        Token(TokenType::TT_NUMERIC, msg, makeInteger(99, true))
    };

    ScamValue expr = runTest(tokens);

    expectInteger(expr, 99, msg, true);
}

TEST_F(ParserTest, NilTest)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectNil(expr);
}

TEST_F(ParserTest, ListTest)
{
    static const string msg{ "(quote 2)" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_SYMBOL, "quote"),
        Token(TokenType::TT_NUMERIC, "2", makeInteger(2, true)),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, UnterminatedList)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
    };

    ScamValue expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, ListWithTokenError)
{
    string const msg { "A message from our sponsors" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_SCAN_ERROR, msg),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectError(expr, msg);
}

TEST_F(ParserTest, DottedPairTest)
{
    static const string msg{ "(5 17 . 2)" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_NUMERIC, "5", makeInteger(5, true)),
        Token(TokenType::TT_NUMERIC, "17", makeInteger(17, true)),
        Token(TokenType::TT_DOT, "."),
        Token(TokenType::TT_NUMERIC, "2", makeInteger(2, true)),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectPair(expr, msg);
}

TEST_F(ParserTest, DottedPairNoCdr)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_NUMERIC, "5", makeInteger(5, true)),
        Token(TokenType::TT_NUMERIC, "17", makeInteger(17, true)),
        Token(TokenType::TT_DOT, "."),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, DottedPairNoClose)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_NUMERIC, "5", makeInteger(5, true)),
        Token(TokenType::TT_DOT, "."),
        Token(TokenType::TT_NUMERIC, "17", makeInteger(17, true)),
    };

    ScamValue expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, DottedPairExcessForms)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_NUMERIC, "5", makeInteger(5, true)),
        Token(TokenType::TT_DOT, "."),
        Token(TokenType::TT_NUMERIC, "17", makeInteger(17, true)),
        Token(TokenType::TT_NUMERIC, "42", makeInteger(42, true)),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, ExtraDot)
{
    vector<Token> tokens {
        Token(TokenType::TT_DOT, "."),
    };

    ScamValue expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, CloseWithoutOpenParen)
{
    vector<Token> tokens {
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, ReaderMacroQuote)
{
    static const string msg{ "(quote foo)" };

    vector<Token> tokens {
        Token(TokenType::TT_QUOTE, "'"),
        Token(TokenType::TT_SYMBOL, "foo")
    };

    ScamValue expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, ReaderMacroQuasiquote)
{
    static const string msg{ "(quasiquote foo)" };

    vector<Token> tokens {
        Token(TokenType::TT_QUASIQUOTE, "`"),
        Token(TokenType::TT_SYMBOL, "foo")
    };

    ScamValue expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, ReaderMacroUnquote)
{
    static const string msg{ "(unquote foo)" };

    vector<Token> tokens {
        Token(TokenType::TT_UNQUOTE, ","),
        Token(TokenType::TT_SYMBOL, "foo")
    };

    ScamValue expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, ReaderMacroUnquoteSplice)
{
    static const string msg{ "(splice foo)" };

    vector<Token> tokens {
        Token(TokenType::TT_SPLICE, ",@"),
        Token(TokenType::TT_SYMBOL, "foo")
    };

    ScamValue expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, ReaderMacroNoForm)
{
    vector<Token> tokens {
        Token(TokenType::TT_SPLICE, ",@"),
    };

    ScamValue expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, ReaderMacroListForm)
{
    string const msg { "(quote (5 42))" };
    vector<Token> tokens {
        Token(TokenType::TT_QUOTE, "'"),
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_NUMERIC, "5", makeInteger(5, true)),
        Token(TokenType::TT_NUMERIC, "42", makeInteger(42, true)),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, VectorEmpty)
{
    string const msg { "#()" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_VECTOR, "#("),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectVector(expr, msg, 0);
}

TEST_F(ParserTest, VectorNonEmpty)
{
    string const msg { "#(5 42)" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_VECTOR, "#("),
        Token(TokenType::TT_NUMERIC, "5", makeInteger(5, true)),
        Token(TokenType::TT_NUMERIC, "42", makeInteger(42, true)),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectVector(expr, msg, 2);
    EXPECT_EQ(42, asInteger(nthcar(expr, 1)));
}

TEST_F(ParserTest, ByteVectorEmpty)
{
    string const msg { "#u8()" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_BYTE_VECTOR, "#u8("),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectByteVector(expr, msg, 0);
}

TEST_F(ParserTest, ByteVectorNonEmpty)
{
    string const msg { "#u8(1 127 38)" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_BYTE_VECTOR, "#u8("),
        Token(TokenType::TT_NUMERIC, "1", makeInteger(1, true)),
        Token(TokenType::TT_NUMERIC, "127", makeInteger(127, true)),
        Token(TokenType::TT_NUMERIC, "38", makeInteger(38, true)),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamValue expr = runTest(tokens);
    expectByteVector(expr, msg, 3);
}

TEST_F(ParserTest, Backtrack)
{
    string const msg { "(backtrack)" };
    vector<Token> tokens {
        Token(TokenType::TT_QUESTION, "?")
    };

    ScamValue expr = runTest(tokens);
    expectList(expr, msg, 1);
}

TEST_F(ParserTest, Keyword)
{
    string const msg { ":test" };
    vector<Token> tokens {
        Token(TokenType::TT_KEYWORD, ":test")
    };

    ScamValue expr = runTest(tokens);
    expectKeyword(expr, msg);
}

TEST_F(ParserTest, DictEmpty)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_CURLY, "{"),
        Token(TokenType::TT_CLOSE_CURLY, "}"),
    };

    ScamValue expr = runTest(tokens);
    expectDict(expr, 0u, "{}");
}

TEST_F(ParserTest, DictNonEmpty)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_CURLY, "{"),
        Token(TokenType::TT_KEYWORD,    ":test"),
        Token(TokenType::TT_NUMERIC, "1", makeInteger(1, true)),
        Token(TokenType::TT_CLOSE_CURLY, "}"),
    };

    ScamValue expr = runTest(tokens);
    expectDict(expr, 1u, "{ :test 1 }");
}
