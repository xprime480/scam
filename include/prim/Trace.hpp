#if ! defined(PRIMITIVE_TRACE_H)
#define PRIMITIVE_TRACE_H 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyTrace(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine);
}

#endif
