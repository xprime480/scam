#if ! defined(READEVALSTRING_HPP)
#define READEVALSTRING_HPP 1

#include "util/ReadEval.hpp"

#include "input/StringTokenizer.hpp"

#include <string>

namespace scam
{
    class ReadEvalString : public ReadEval
    {
    public:
        ReadEvalString(std::string const & text);

    private:
        StringTokenizer tokenizer;
    };
}

#endif
