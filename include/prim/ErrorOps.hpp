#if ! defined(ERROROPS_HPP)
#define ERROROPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyError(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine);

    extern void applyRaise(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine);
}

#endif
