#if ! defined(READEVALSTREAM_HPP)
#define READEVALSTREAM_HPP 1

#include "util/ReadEval.hpp"

#include "input/CharStreamTokenizer.hpp"

namespace scam
{
    class CharStream;

    class ReadEvalStream : public ReadEval
    {
    public:
        ReadEvalStream(CharStream & stream);

    private:
        CharStreamTokenizer tokenizer;
    };
}

#endif
