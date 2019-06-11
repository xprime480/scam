#if ! defined(CHARSTREAMTOKENIZER_H)
#define CHARSTREAMTOKENIZER_H 1

#include "input/Tokenizer.hpp"

#include "input/StringCharStream.hpp"
#include "input/Token.hpp"

#include <string>


namespace scam
{
    class CharStreamTokenizer : public Tokenizer
    {
    public:
        CharStreamTokenizer(CharStream & stream);
        ~CharStreamTokenizer();

        void mark() const override;
        Token next() override;

    private:
        CharStream & stream;
        bool ok;

        Token scanAtmosphere();
        bool skipWhitespace();
        bool skipSimpleComments();
        Token skipNestedComments();

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

