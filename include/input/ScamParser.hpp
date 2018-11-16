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

        ExprHandle parseExpr() const;

    private:
        Tokenizer & tokenizer;

        ExprHandle tokenToExpr(Token const & token) const;

        ExprHandle parseSubExpr() const;

        ExprHandle parseList() const;
        ExprHandle parseDotContext() const;

        ExprHandle parseVector() const;

        ExprHandle expand_reader_macro(std::string const & text) const;
    };
}

#endif
