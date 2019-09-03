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
        ScamRepl(int argc, char ** argv);
        int run();

    private:
        std::vector<std::string> preloads;

        ScamEngine & engine;
        ReplTokenizer tokenizer;
        ScamParser parser;

        bool testmode;
        bool done;

        void readArgs(int argc, char ** argv);

        void banner() const;
        int load_preloads();
        ScamValue evaluateFile(const std::string & name);
        int checkResult(const std::string & file, ScamValue result);
        int repl();

        ScamValue read();
        ScamValue eval(ScamValue form);
        void print(ScamValue value);

        void checkInternal(std::string & line);
    };
}

#endif
