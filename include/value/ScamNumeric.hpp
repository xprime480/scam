#if ! defined(SCAMNUMERIC_H)
#define SCAMNUMERIC_H 1

#include "value/ScamData.hpp"

namespace scam
{
    extern ScamValue realPart(ScamValue data);
    extern ScamValue imagPart(ScamValue data);
}

#endif
