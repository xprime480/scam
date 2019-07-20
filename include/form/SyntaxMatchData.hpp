#if ! defined(SYNTAXMATCHDATA_HPP)
#define SYNTAXMATCHDATA_HPP 1

#include "ScamFwd.hpp"

#include <map>
#include <string>

namespace scam
{
    struct SyntaxMatchData
    {
	std::map<std::string, ScamValue> data;
    };
}

#endif
