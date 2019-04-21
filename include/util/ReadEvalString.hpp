#if ! defined(READEVALSTRING_HPP)
#define READEVALSTRING_HPP 1

#include "Extractor.hpp"
#include "ScamEngine.hpp"
#include "input/StringTokenizer.hpp"

#include <string>
#include <vector>

namespace scam
{
    class ReadEvalString
    {
    public:
        ReadEvalString(ScamEngine * engine, std::string const & text);
        ~ReadEvalString();

        ScamExpr * run();
        ScamExpr * read();

    private:
        ScamEngine * engine;
        StringTokenizer tokenizer;
    };
}

#endif
