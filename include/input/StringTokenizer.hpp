#if ! defined(STRINGTOKENIZER_H)
#define STRINGTOKENIZER_H 1

#include "input/Tokenizer.hpp"

#include <string>

#include "input/Token.hpp"

namespace scam
{
    class StringTokenizer : public Tokenizer
    {
    public:
        StringTokenizer(std::string const & input);

        Token next() override;

    private:
        std::string const & input;
        char const * pos;

        void skipWhitespace();
        bool isDelimiter(char c) const;

        Token scanBoolean();

        TokenType scanNumericToken(std::string & contents);
        TokenType scanString(std::string & contents);
        TokenType scanSymbol(std::string & contents);
    };
}

#endif

