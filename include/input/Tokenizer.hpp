#if ! defined(TOKENIZER_H)
#define TOKENIZER_H 1

#include "input/Token.hpp"

#include <iostream>

namespace scam
{
    class Tokenizer
    {
    public:
        virtual ~Tokenizer() {}
        virtual Token next() = 0;
    };
}

#endif
