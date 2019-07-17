#if ! defined(VALUEWRITER_HPP)
#define VALUEWRITER_HPP 1

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    extern std::string debugWriteValue(ScamValue data);
    extern std::string writeValue(ScamValue data);
    extern std::string describe(DataTagType type);
}

#endif
