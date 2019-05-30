#if ! defined(SCAMTOINTERNAL_HPP)
#define SCAMTOINTERNAL_HPP 1

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    class ScamData;

    struct RationalPair
    {
        int num;
        int den;
    };

    extern char asChar(ScamValue data);
    extern std::string asString(ScamValue data);
    extern double asDouble(ScamValue data);
    extern RationalPair asRational(ScamValue data);
    extern int asInteger(ScamValue data);
}


#endif
