#if ! defined(PORTOPS_HPP)
#define PORTOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyOpenInStr(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine);
}

#endif
