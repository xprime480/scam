
#include "Continuation.hpp"

#include "expr/ScamExpr.hpp"

#include "input/ScamParser.hpp"
#include "input/Tokenizer.hpp"

#include "Extractor.hpp"

#include "gtest/gtest.h"

#include <functional>
#include <vector>

using namespace scam;
using namespace std;

namespace
{
    static const Token eof(TokenType::TT_END_OF_INPUT, "");

    class StaticTokenizer : public Tokenizer
    {
    public:
        StaticTokenizer(vector<Token> const & tokens)
            : tokens(tokens)
            , index(0)
        {
        }

        Token next() override
        {
            if ( index >= tokens.size() ) {
                return eof;
            }

            Token const & token = tokens[index];
            ++index;
            return token;
        }

    private:
        vector<Token> const & tokens;
        size_t index;
    };

    ExprHandle runTest(vector<Token> const & tokens)
    {
        StaticTokenizer tokenizer(tokens);
        ScamParser parser(tokenizer);
        shared_ptr<Extractor> ec = make_shared<Extractor>();
        parser.parseExpr(ec);

        ExprHandle expr = ec->getExpr();
        EXPECT_NE(nullptr, expr.get());
        return expr;
    }

    TEST(ParserTest, NoneToken)
    {
        vector<Token> tokens {
            Token(TokenType::TT_NONE, "")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
    }

    TEST(ParserTest, EndOfInput)
    {
        vector<Token> tokens {
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->isNull());
    }

    TEST(ParserTest, ScanError)
    {
        string const msg{ "blah" };
        vector<Token> tokens {
            Token(TokenType::TT_SCAN_ERROR, msg)
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
        EXPECT_EQ(msg, expr->toString());
    }

    void booltest(string const & msg, bool value)
    {
        vector<Token> tokens {
            Token(TokenType::TT_BOOLEAN, msg)
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_EQ(value, expr->truth());
    }

    TEST(ParserTest, BoolTrue)
    {
        booltest("#t", true);
    }

    TEST(ParserTest, BoolFalse)
    {
        booltest("#f", false);
    }

    TEST(ParserTest, CharacterTest)
    {
        static const string msg{ "\\#Z" };
        vector<Token> tokens {
            Token(TokenType::TT_CHARACTER, msg)
        };

        ExprHandle expr = runTest(tokens);

        EXPECT_TRUE(expr->isChar());
        EXPECT_EQ(msg, expr->toString());
        EXPECT_EQ('Z', expr->toChar());
    }

    TEST(ParserTest, StringTest)
    {
        static const string msg{ "Holy Test Coverage, Batman!" };
        vector<Token> tokens {
            Token(TokenType::TT_STRING, msg)
        };

        ExprHandle expr = runTest(tokens);

        EXPECT_TRUE(expr->isString());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, SymbolTest)
    {
        static const string msg{ "nil?" };
        vector<Token> tokens {
            Token(TokenType::TT_SYMBOL, msg)
        };

        ExprHandle expr = runTest(tokens);

        EXPECT_TRUE(expr->isSymbol());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, FloatTest)
    {
        static const string msg{ "-17.5" };
        vector<Token> tokens {
            Token(TokenType::TT_FLOAT, msg)
        };

        ExprHandle expr = runTest(tokens);

        EXPECT_TRUE(expr->isFloat());
        EXPECT_EQ(-17.5, expr->toFloat());
    }

    TEST(ParserTest, IntegerTest)
    {
        static const string msg{ "99" };
        vector<Token> tokens {
            Token(TokenType::TT_INTEGER, msg)
        };

        ExprHandle expr = runTest(tokens);

        EXPECT_TRUE(expr->isFloat());
        EXPECT_TRUE(expr->isInteger());
        EXPECT_EQ(99, expr->toInteger());
    }

    TEST(ParserTest, NilTest)
    {
        static const string msg{ "()" };
        vector<Token> tokens {
            Token(TokenType::TT_OPEN_PAREN, "("),
            Token(TokenType::TT_CLOSE_PAREN, ")")
        };

        ExprHandle expr = runTest(tokens);

        EXPECT_TRUE(expr->isNil());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, ListTest)
    {
        static const string msg{ "(quote 2)" };
        vector<Token> tokens {
            Token(TokenType::TT_OPEN_PAREN, "("),
            Token(TokenType::TT_SYMBOL, "quote"),
            Token(TokenType::TT_INTEGER, "2"),
            Token(TokenType::TT_CLOSE_PAREN, ")")
        };

        ExprHandle expr = runTest(tokens);

        EXPECT_TRUE(expr->isList());
        EXPECT_TRUE(expr->isCons());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, UnterminatedList)
    {
        vector<Token> tokens {
            Token(TokenType::TT_OPEN_PAREN, "("),
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
    }

    TEST(ParserTest, ListWithTokenError)
    {
        string const msg { "A message from our sponsors" };
        vector<Token> tokens {
            Token(TokenType::TT_OPEN_PAREN, "("),
            Token(TokenType::TT_SCAN_ERROR, msg),
            Token(TokenType::TT_CLOSE_PAREN, ")")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, DottedPairTest)
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

        ExprHandle expr = runTest(tokens);

        EXPECT_FALSE(expr->isList());
        EXPECT_TRUE(expr->isCons());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, DottedPairNoCdr)
    {
        vector<Token> tokens {
            Token(TokenType::TT_OPEN_PAREN, "("),
            Token(TokenType::TT_INTEGER, "5"),
            Token(TokenType::TT_INTEGER, "17"),
            Token(TokenType::TT_DOT, "."),
            Token(TokenType::TT_CLOSE_PAREN, ")")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
    }

    TEST(ParserTest, DottedPairNoClose)
    {
        vector<Token> tokens {
            Token(TokenType::TT_OPEN_PAREN, "("),
            Token(TokenType::TT_INTEGER, "5"),
            Token(TokenType::TT_DOT, "."),
            Token(TokenType::TT_INTEGER, "17"),
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
    }

    TEST(ParserTest, DottedPairExcessForms)
    {
        vector<Token> tokens {
            Token(TokenType::TT_OPEN_PAREN, "("),
            Token(TokenType::TT_INTEGER, "5"),
            Token(TokenType::TT_DOT, "."),
            Token(TokenType::TT_INTEGER, "17"),
            Token(TokenType::TT_INTEGER, "42"),
            Token(TokenType::TT_CLOSE_PAREN, ")")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
    }

    TEST(ParserTest, ExtraDot)
    {
        vector<Token> tokens {
            Token(TokenType::TT_DOT, "."),
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
    }

    TEST(ParserTest, CloseWithoutOpenParen)
    {
        vector<Token> tokens {
            Token(TokenType::TT_CLOSE_PAREN, ")")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
    }

    TEST(ParserTest, ReaderMacroQuote)
    {
        static const string msg{ "(quote foo)" };

        vector<Token> tokens {
            Token(TokenType::TT_QUOTE, "'"),
            Token(TokenType::TT_SYMBOL, "foo")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->isList());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, ReaderMacroQuasiquote)
    {
        static const string msg{ "(quasiquote foo)" };

        vector<Token> tokens {
            Token(TokenType::TT_QUASIQUOTE, "`"),
            Token(TokenType::TT_SYMBOL, "foo")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->isList());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, ReaderMacroUnquote)
    {
        static const string msg{ "(unquote foo)" };

        vector<Token> tokens {
            Token(TokenType::TT_UNQUOTE, ","),
            Token(TokenType::TT_SYMBOL, "foo")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->isList());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, ReaderMacroUnquoteSplice)
    {
        static const string msg{ "(splice foo)" };

        vector<Token> tokens {
            Token(TokenType::TT_SPLICE, ",@"),
            Token(TokenType::TT_SYMBOL, "foo")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->isList());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, ReaderMacroNoForm)
    {
        vector<Token> tokens {
            Token(TokenType::TT_SPLICE, ",@"),
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
    }

    TEST(ParserTest, ReaderMacroListForm)
    {
        string const msg { "(quote (5 42))" };
        vector<Token> tokens {
            Token(TokenType::TT_QUOTE, "'"),
            Token(TokenType::TT_OPEN_PAREN, "("),
            Token(TokenType::TT_INTEGER, "5"),
            Token(TokenType::TT_INTEGER, "42"),
            Token(TokenType::TT_CLOSE_PAREN, ")")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->isList());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, VectorEmpty)
    {
        string const msg { "[]" };
        vector<Token> tokens {
            Token(TokenType::TT_OPEN_BRACKET, "["),
            Token(TokenType::TT_CLOSE_BRACKET, "]")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->isVector());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, VectorNonEmpty)
    {
        string const msg { "[5 42]" };
        vector<Token> tokens {
            Token(TokenType::TT_OPEN_BRACKET, "["),
            Token(TokenType::TT_INTEGER, "5"),
            Token(TokenType::TT_INTEGER, "42"),
            Token(TokenType::TT_CLOSE_BRACKET, "]")
        };

        ExprHandle expr = runTest(tokens);
        EXPECT_TRUE(expr->isVector());
        EXPECT_EQ(msg, expr->toString());
        EXPECT_EQ(42, expr->nthcar(1)->toInteger());
    }
}
