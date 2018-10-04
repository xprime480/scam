
#include "ScamContext.hpp"
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

    shared_ptr<ScamExpr> runTest(vector<Token> const & tokens)
    {
        StaticTokenizer tokenizer(tokens);
        ScamParser parser(tokenizer);
        shared_ptr<Extractor> ec = make_shared<Extractor>();
        ScamContext sc { ec };
        parser.parseExpr(sc);

        shared_ptr<ScamExpr> expr = ec->getExpr();
        EXPECT_NE(nullptr, expr.get());
        return expr;
    }

    TEST(ParserTest, NoneToken)
    {
        vector<Token> tokens {
            Token(TokenType::TT_NONE, "")
        };

        shared_ptr<ScamExpr> expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
    }

    TEST(ParserTest, EndOfInput)
    {
        vector<Token> tokens {
        };

        shared_ptr<ScamExpr> expr = runTest(tokens);
        EXPECT_TRUE(expr->isNull());
    }

    TEST(ParserTest, ScanError)
    {
        string const msg{ "blah" };
        vector<Token> tokens {
            Token(TokenType::TT_SCAN_ERROR, msg)
        };

        shared_ptr<ScamExpr> expr = runTest(tokens);
        EXPECT_TRUE(expr->error());
        EXPECT_EQ(msg, expr->toString());
    }

    void booltest(string const & msg, bool value)
    {
        vector<Token> tokens {
            Token(TokenType::TT_BOOLEAN, msg)
        };

        shared_ptr<ScamExpr> expr = runTest(tokens);
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

        shared_ptr<ScamExpr> expr = runTest(tokens);

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

        shared_ptr<ScamExpr> expr = runTest(tokens);

        EXPECT_TRUE(expr->isString());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, SymbolTest)
    {
        static const string msg{ "nil?" };
        vector<Token> tokens {
            Token(TokenType::TT_SYMBOL, msg)
        };

        shared_ptr<ScamExpr> expr = runTest(tokens);

        EXPECT_TRUE(expr->isSymbol());
        EXPECT_EQ(msg, expr->toString());
    }

    TEST(ParserTest, FloatTest)
    {
        static const string msg{ "-17.5" };
        vector<Token> tokens {
            Token(TokenType::TT_FLOAT, msg)
        };

        shared_ptr<ScamExpr> expr = runTest(tokens);

        EXPECT_TRUE(expr->isFloat());
        EXPECT_EQ(-17.5, expr->toFloat());
    }

    TEST(ParserTest, IntegerTest)
    {
        static const string msg{ "99" };
        vector<Token> tokens {
            Token(TokenType::TT_INTEGER, msg)
        };

        shared_ptr<ScamExpr> expr = runTest(tokens);

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

        shared_ptr<ScamExpr> expr = runTest(tokens);

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

        shared_ptr<ScamExpr> expr = runTest(tokens);

        EXPECT_TRUE(expr->isList());
        EXPECT_TRUE(expr->isCons());
        EXPECT_EQ(msg, expr->toString());
    }

}
