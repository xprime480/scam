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
        ~StringTokenizer();

        Token next() override;

    private:
        std::string const input;
        char const * pos;

        Token scanAtmosphere();
        bool skipWhitespace();
        bool skipSimpleComments();
        Token skipNestedComments();

        bool isDelimiter(char c) const;
        bool isIdentifierCharacter(char c) const;

        Token scanSpecial();
        Token scanBoolean();
        Token scanCharacter();
        Token scanString();
        Token scanNumeric();
        Token scanKeyword();
        Token scanSymbol();
        Token scanDelimitedSymbol();
    };
}

#endif

