#if ! defined(SCAMENGINE_H)
#define SCAMENGINE_H 1

#include "input/Tokenizer.hpp"
#include "output/OutputHandler.hpp"

#include <string>
#include <memory>

namespace scam
{
    class ScamEngine
    {
    public:
        ScamEngine();
        void repl(Tokenizer & input, OutputHandler & output);

        void extend(std::string const & name,
                    Tokenizer & input,
                    OutputHandler & output);
    };
}

#endif

