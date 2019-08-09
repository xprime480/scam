#if ! defined(READEVAL_HPP)
#define READEVAL_HPP 1

#include "input/StringTokenizer.hpp"

#include <string>

namespace scam
{
    class ScamEngine;
    class Tokenizer;

    class ReadEval
    {
    public:
        ReadEval(ScamEngine * engine, Tokenizer & tokenizer);
        virtual ~ReadEval();

        ScamValue run();
        ScamValue read();

    private:
        ScamEngine * engine;
        Tokenizer & tokenizer;
    };
}

#endif
