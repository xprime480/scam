#if ! defined(VALUEWRITER_HPP)
#define VALUEWRITER_HPP 1

#include "ScamFwd.hpp"
#include "value/ScamValueType.hpp"

#include <string>

namespace scam
{
    extern std::string debugWriteValue(ScamValue data);

    extern std::string
    writeValue(ScamValue data, bool suppressExactness = false);

    extern std::string describe(ScamValueType type);
}

#endif
