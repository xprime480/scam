#if ! defined(SCAMREPL_HPP)
#define SCAMREPL_HPP 1

#include "ScamEngine.hpp"
#include "ReplTokenizer.hpp"

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

        void banner() const;
        bool load_prelude();
        int repl();

        ExprHandle read();
        ExprHandle eval(ExprHandle form);
        void print(ExprHandle value);

        bool checkInternal(std::string & line);
    };
}

#endif
