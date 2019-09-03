#if ! defined(READEVAL_HPP)
#define READEVAL_HPP 1

#include "input/StringTokenizer.hpp"

#include <string>

namespace scam
{
    class Tokenizer;

    class ReadEval
    {
    public:
        ReadEval(Tokenizer & tokenizer);
        virtual ~ReadEval();

        ScamValue run();
        ScamValue read();

    private:
        Tokenizer & tokenizer;
    };
}

#endif
