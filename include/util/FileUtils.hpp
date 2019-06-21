#if ! defined(FILEUTILS_HPP)
#define FILEUTILS_HPP

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    extern std::string findFileOnPath(const std::string & filename);

    extern ScamValue
    loadEvalFile(const std::string & fullpath, ScamEngine * engine);
}

#endif
