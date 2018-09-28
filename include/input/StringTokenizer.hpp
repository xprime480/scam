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

        bool skipWhitespace();
        bool skipSimpleComments();
        Token skipNestedComments();

        bool isDelimiter(char c) const;

        Token scanBoolean();
        Token scanInteger();
    };
}

#endif

