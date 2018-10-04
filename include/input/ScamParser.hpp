#if ! defined(SCAMPARSER_H)
#define SCAMPARSER_H 1

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"

#include "input/Tokenizer.hpp"

#include <string>

namespace scam
{
    class Token;
    class ScamContext;

    class ScamParser
    {
    public:

        ScamParser(Tokenizer & tokenizer);

        void parseExpr(ScamContext & context) const;

    private:
        Tokenizer & tokenizer;

        std::shared_ptr<ScamExpr> tokenToExpr(Token const & token) const;

        std::shared_ptr<ScamExpr> parseSubExpr() const;

        std::shared_ptr<ScamExpr> parseList() const;
        std::shared_ptr<ScamExpr> parseDotContext() const;

        std::shared_ptr<ScamExpr> parseVector() const;

        std::shared_ptr<ScamExpr>
        expand_reader_macro(std::string const & text) const;
    };
}

#endif
