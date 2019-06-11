#if ! defined(STRINGTOKENIZER_H)
#define STRINGTOKENIZER_H 1

#include "input/Tokenizer.hpp"

#include "input/CharStreamTokenizer.hpp"
#include "input/StringCharStream.hpp"
#include "input/Token.hpp"

#include <string>

namespace scam
{
    class StringTokenizer : public Tokenizer
    {
    public:
        StringTokenizer(std::string const & input);

        void mark() const override;

        Token next() override;

    private:
        StringCharStream stream;
        CharStreamTokenizer tokenizer;
    };
}

#endif

