#if ! defined(PAIROPS_HPP)
#define PAIROPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyCar(ScamValue args, Continuation * cont);
    extern void applyCdr(ScamValue args, Continuation * cont);
    extern void applyCons(ScamValue args, Continuation * cont);
    extern void applySetCarX(ScamValue args, Continuation * cont);
    extern void applySetCdrX(ScamValue args, Continuation * cont);
}

#endif
