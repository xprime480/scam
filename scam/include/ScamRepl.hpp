#if ! defined(SCAMREPL_HPP)
#define SCAMREPL_HPP 1

#include "ScamEngine.hpp"
#include "ReplTokenizer.hpp"

#include <string>

namespace scam
{
    class ScamRepl
    {
    public:
        ScamRepl();
        int run();

    private:
        ScamEngine engine;
        ReplTokenizer tokenizer;
        ScamParser parser;
        bool done;

        void banner() const;
        bool load_prelude();
        int repl();

        ScamValue read();
        ScamValue eval(ScamValue form);
        void print(ScamValue value);

        void checkInternal(std::string & line);
    };
}

#endif
