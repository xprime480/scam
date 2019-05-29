#if ! defined(PRIMITIVE_INCLUDE_H)
#define PRIMITIVE_INCLUDE_H 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyInclude(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine);
}

#endif
