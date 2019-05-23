#if ! defined(SCAMTOINTERNAL_HPP)
#define SCAMTOINTERNAL_HPP 1

#include "expr/ScamData.hpp"

namespace scam
{
    class ScamData;

    struct RationalPair
    {
        int num;
        int den;
    };

    extern char asChar(const ScamData * data);
    extern double asDouble(const ScamData * data);
    extern RationalPair asRational(const ScamData * data);
    extern int asInteger(const ScamData * data);
}


#endif
