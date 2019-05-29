#if ! defined(PRIMITIVE_ERROR_H)
#define PRIMITIVE_ERROR_H 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyError(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine);
}

#endif
