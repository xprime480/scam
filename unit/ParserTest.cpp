
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

        EXPECT_FALSE(expr->isNull());
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
        EXPECT_FALSE(expr->isNull());
        EXPECT_TRUE(expr->error());
        EXPECT_EQ(msg, expr->toString());
    }

    void booltest(string const & msg, bool value)
    {
        vector<Token> tokens {
            Token(TokenType::TT_BOOLEAN, msg)
        };

        shared_ptr<ScamExpr> expr = runTest(tokens);
        EXPECT_FALSE(expr->isNull());
        EXPECT_FALSE(expr->error());
        EXPECT_EQ(msg, expr->toString());
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

    TEST(ParserTest, FloatTest)
    {
        static const string msg{ "-17.5" };
        vector<Token> tokens {
            Token(TokenType::TT_FLOAT, msg)
        };

        shared_ptr<ScamExpr> expr = runTest(tokens);

        EXPECT_FALSE(expr->isNull());
        EXPECT_FALSE(expr->error());
        EXPECT_TRUE(expr->isNumeric());
        EXPECT_TRUE(expr->isFloat());
        EXPECT_EQ(msg, expr->toString());
        EXPECT_EQ(-17.5, expr->toFloat());
    }
}
