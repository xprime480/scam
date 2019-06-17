#if ! defined(SCAM_REPLTOKENIZER_HPP)
#define SCAM_REPLTOKENIZER_HPP 1

#include "input/Tokenizer.hpp"

#include <vector>

namespace scam
{
    class ReplTokenizer : public Tokenizer
    {
    public:
        ReplTokenizer();

        void mark() override;

        Token next() override;

        void bufferInput(std::string input);

        void flush();
        void restart();

        bool empty() const;

    private:
        std::vector<Token> buffer;
        size_t pos;
    };
}

#endif
