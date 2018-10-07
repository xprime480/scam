#if ! defined(SCAMENGINE_H)
#define SCAMENGINE_H 1

#include "Env.hpp"
#include "input/Tokenizer.hpp"
#include "output/OutputHandler.hpp"

#include <string>

namespace scam
{
    class ScamEngine
    {
    public:
        ScamEngine();

        static Env getStandardEnv();

        void repl(Tokenizer & input, OutputHandler & output);

        void extend(std::string const & name,
                    Tokenizer & input,
                    OutputHandler & output);
    };
}

#endif

