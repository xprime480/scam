
#include "TestBase.hpp"
#include "StaticTokenizer.hpp"

#include "input/ScamParser.hpp"

#include <vector>

using namespace scam;
using namespace scam::test_impl;
using namespace std;

namespace scam
{
    class ScamExpr;
}

class ParserTest : public TestBase
{
protected:
    ScamExpr * runTest(vector<Token> const & tokens)
    {
        StaticTokenizer tokenizer(tokens);
        ScamParser parser(tokenizer);
        ScamExpr * expr = parser.parseExpr();
        EXPECT_NE(nullptr, expr);
        return expr;
    }

    void booltest(string const & msg, bool value)
    {
        vector<Token> tokens {
            Token(TokenType::TT_BOOLEAN, msg)
        };

        ScamExpr * expr = runTest(tokens);
        expectBoolean(expr, value, msg);
    }
};

TEST_F(ParserTest, NoneToken)
{
    vector<Token> tokens {
        Token(TokenType::TT_NONE, "")
    };

    ScamExpr * expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, EndOfInput)
{
    vector<Token> tokens {
    };

    ScamExpr * expr = runTest(tokens);
    expectNull(expr);
}

TEST_F(ParserTest, ScanError)
{
    string const msg{ "blah" };
    vector<Token> tokens {
        Token(TokenType::TT_SCAN_ERROR, msg)
    };

    ScamExpr * expr = runTest(tokens);
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
    static const string msg{ "\\#Z" };
    vector<Token> tokens {
        Token(TokenType::TT_CHARACTER, msg)
    };

    ScamExpr * expr = runTest(tokens);
    expectChar(expr, 'Z', msg);
}

TEST_F(ParserTest, StringTest)
{
    static const string msg{ "Holy Test Coverage, Batman!" };
    vector<Token> tokens {
        Token(TokenType::TT_STRING, msg)
    };

    ScamExpr * expr = runTest(tokens);
    expectString(expr, msg);
}

TEST_F(ParserTest, SymbolTest)
{
    static const string msg{ "nil?" };
    vector<Token> tokens {
        Token(TokenType::TT_SYMBOL, msg)
    };

    ScamExpr * expr = runTest(tokens);
    expectSymbol(expr, msg);
}

TEST_F(ParserTest, FloatTest)
{
    static const string msg{ "-17.5" };
    vector<Token> tokens {
        Token(TokenType::TT_FLOAT, msg)
    };

    ScamExpr * expr = runTest(tokens);
    expectFloat(expr, -17.5, msg);
}

TEST_F(ParserTest, IntegerTest)
{
    static const string msg{ "99" };
    vector<Token> tokens {
        Token(TokenType::TT_INTEGER, msg)
    };

    ScamExpr * expr = runTest(tokens);

    expectInteger(expr, 99, msg);
}

TEST_F(ParserTest, NilTest)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamExpr * expr = runTest(tokens);
    expectNil(expr);
}

TEST_F(ParserTest, ListTest)
{
    static const string msg{ "(quote 2)" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_SYMBOL, "quote"),
        Token(TokenType::TT_INTEGER, "2"),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamExpr * expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, UnterminatedList)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
    };

    ScamExpr * expr = runTest(tokens);
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

    ScamExpr * expr = runTest(tokens);
    expectError(expr, msg);
}

TEST_F(ParserTest, DottedPairTest)
{
    static const string msg{ "(5 17 . 2)" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_INTEGER, "5"),
        Token(TokenType::TT_INTEGER, "17"),
        Token(TokenType::TT_DOT, "."),
        Token(TokenType::TT_INTEGER, "2"),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamExpr * expr = runTest(tokens);
    expectCons(expr, msg);
}

TEST_F(ParserTest, DottedPairNoCdr)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
            Token(TokenType::TT_INTEGER, "5"),
            Token(TokenType::TT_INTEGER, "17"),
            Token(TokenType::TT_DOT, "."),
            Token(TokenType::TT_CLOSE_PAREN, ")")
            };

    ScamExpr * expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, DottedPairNoClose)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_INTEGER, "5"),
        Token(TokenType::TT_DOT, "."),
        Token(TokenType::TT_INTEGER, "17"),
    };

    ScamExpr * expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, DottedPairExcessForms)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_INTEGER, "5"),
        Token(TokenType::TT_DOT, "."),
        Token(TokenType::TT_INTEGER, "17"),
        Token(TokenType::TT_INTEGER, "42"),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamExpr * expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, ExtraDot)
{
    vector<Token> tokens {
        Token(TokenType::TT_DOT, "."),
    };

    ScamExpr * expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, CloseWithoutOpenParen)
{
    vector<Token> tokens {
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamExpr * expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, ReaderMacroQuote)
{
    static const string msg{ "(quote foo)" };

    vector<Token> tokens {
        Token(TokenType::TT_QUOTE, "'"),
        Token(TokenType::TT_SYMBOL, "foo")
    };

    ScamExpr * expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, ReaderMacroQuasiquote)
{
    static const string msg{ "(quasiquote foo)" };

    vector<Token> tokens {
        Token(TokenType::TT_QUASIQUOTE, "`"),
        Token(TokenType::TT_SYMBOL, "foo")
    };

    ScamExpr * expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, ReaderMacroUnquote)
{
    static const string msg{ "(unquote foo)" };

    vector<Token> tokens {
        Token(TokenType::TT_UNQUOTE, ","),
        Token(TokenType::TT_SYMBOL, "foo")
    };

    ScamExpr * expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, ReaderMacroUnquoteSplice)
{
    static const string msg{ "(splice foo)" };

    vector<Token> tokens {
        Token(TokenType::TT_SPLICE, ",@"),
        Token(TokenType::TT_SYMBOL, "foo")
    };

    ScamExpr * expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, ReaderMacroNoForm)
{
    vector<Token> tokens {
        Token(TokenType::TT_SPLICE, ",@"),
    };

    ScamExpr * expr = runTest(tokens);
    expectError(expr);
}

TEST_F(ParserTest, ReaderMacroListForm)
{
    string const msg { "(quote (5 42))" };
    vector<Token> tokens {
        Token(TokenType::TT_QUOTE, "'"),
        Token(TokenType::TT_OPEN_PAREN, "("),
        Token(TokenType::TT_INTEGER, "5"),
        Token(TokenType::TT_INTEGER, "42"),
        Token(TokenType::TT_CLOSE_PAREN, ")")
    };

    ScamExpr * expr = runTest(tokens);
    expectList(expr, msg, 2);
}

TEST_F(ParserTest, VectorEmpty)
{
    string const msg { "[]" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_BRACKET, "["),
        Token(TokenType::TT_CLOSE_BRACKET, "]")
    };

    ScamExpr * expr = runTest(tokens);
    expectVector(expr, msg, 0);
}

TEST_F(ParserTest, VectorNonEmpty)
{
    string const msg { "[5 42]" };
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_BRACKET, "["),
        Token(TokenType::TT_INTEGER, "5"),
        Token(TokenType::TT_INTEGER, "42"),
        Token(TokenType::TT_CLOSE_BRACKET, "]")
    };

    ScamExpr * expr = runTest(tokens);
    expectVector(expr, msg, 2);
    EXPECT_EQ(42, expr->nthcar(1)->toInteger());
}

TEST_F(ParserTest, Backtrack)
{
    string const msg { "(backtrack)" };
    vector<Token> tokens {
        Token(TokenType::TT_QUESTION, "?")
    };

    ScamExpr * expr = runTest(tokens);
    expectList(expr, msg, 1);
}

TEST_F(ParserTest, Keyword)
{
    string const msg { ":test" };
    vector<Token> tokens {
        Token(TokenType::TT_KEYWORD, ":test")
    };

    ScamExpr * expr = runTest(tokens);
    expectKeyword(expr, msg);
}

TEST_F(ParserTest, DictEmpty)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_CURLY, "{"),
        Token(TokenType::TT_CLOSE_CURLY, "}"),
    };

    ScamExpr * expr = runTest(tokens);
    expectDict(expr, 0u, "{}");
}

TEST_F(ParserTest, DictNonEmpty)
{
    vector<Token> tokens {
        Token(TokenType::TT_OPEN_CURLY, "{"),
        Token(TokenType::TT_KEYWORD,    ":test"),
        Token(TokenType::TT_INTEGER,    "1"),
        Token(TokenType::TT_CLOSE_CURLY, "}"),
    };

    ScamExpr * expr = runTest(tokens);
    expectDict(expr, 1u, "{ :test 1 }");
}
