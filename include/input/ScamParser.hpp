#if ! defined(SCAMPARSER_H)
#define SCAMPARSER_H 1

#include "ScamFwd.hpp"
#include "input/Tokenizer.hpp"

#include <string>

namespace scam
{
    extern const ScamValue readErrorCategory;

    class Token;

    class ScamParser
    {
    public:
        ScamParser(Tokenizer & tokenizer);

        void mark();

        ScamValue parseExpr() const;

    private:
        Tokenizer & tokenizer;

        ScamValue tokenToExpr(Token const & token) const;

        ScamValue parseSubExpr() const;
        ScamValue parseList() const;
        ScamValue parseDotContext() const;
        ScamValue parseVector() const;
        ScamValue parseByteVector() const;
        ScamValue parseDict() const;
        ScamValue expand_reader_macro(std::string const & text) const;
    };
}

#endif
