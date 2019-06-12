#if ! defined(PAIROPS_HPP)
#define PAIROPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyCar(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine);

    extern void applyCdr(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine);

    extern void applyCons(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine);

    extern void applySetCarX(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine);

    extern void applySetCdrX(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine);

}

#endif
