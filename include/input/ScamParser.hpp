#if ! defined(SCAMPARSER_H)
#define SCAMPARSER_H 1

#include "expr/ScamExpr.hpp"

#include "input/Tokenizer.hpp"

#include <string>

namespace scam
{
    class Token;

    class ScamParser
    {
    public:
        ScamParser(Tokenizer & tokenizer);

        ScamExpr * parseExpr() const;

    private:
        Tokenizer & tokenizer;

        ScamExpr * tokenToExpr(Token const & token) const;

        ScamExpr * parseSubExpr() const;

        ScamExpr * parseList() const;
        ScamExpr * parseDotContext() const;

        ScamExpr * parseVector() const;
        ScamExpr * parseDict() const;

        ScamExpr * expand_reader_macro(std::string const & text) const;
    };
}

#endif
