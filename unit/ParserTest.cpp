
#include "ScamContext.hpp"
#include "Continuation.hpp"

#include "expr/ScamExpr.hpp"

#include "input/ScamParser.hpp"
#include "input/Tokenizer.hpp"

#include <functional>
#include <vector>

using namespace scam;
using namespace std;

namespace
{
    using CheckFunc = function<bool(ScamExpr const &)>;

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

    template <typename T>
    class ExtractionContinuation : public Continuation
    {
    public:
        ExtractionContinuation(T check)
            : check(check)
        {
        }

        void run(std::shared_ptr<ScamExpr> expr) const override
        {
            check(*expr);
        }

    private:
        T check;
    };

    template <typename T>
    void runTest(vector<Token> const & tokens, T check)
    {
        StaticTokenizer tokenizer(tokens);
        ScamParser parser(tokenizer);
        ScamContext sc { make_shared<ExtractionContinuation<T>>(check) };
        parser.parseExpr(sc);
    }

    bool noinput()
    {
        bool pass { false };
        vector<Token> tokens {  };
        auto check = [&pass](ScamExpr const & expr) {
            pass = expr.isNull();
        };

        runTest(tokens, check);
        return pass;
    }

    bool scanerror()
    {
        bool pass { false };
        string const msg{ "blah" };

        vector<Token> tokens {
            Token(TokenType::TT_SCAN_ERROR, msg)
        };
        auto check = [&](ScamExpr const & expr) {
            pass = ! expr.isNull();
            pass &= expr.error();
            pass &= (msg == expr.toString());
        };

        runTest(tokens, check);
        return pass;
    }

}

bool parsertest()
{
    bool ok { true };

    ok &= noinput();
    ok &= scanerror();

    

    return ok;
}
