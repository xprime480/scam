#if ! defined(EQUIVOPS_HPP)
#define EQUIVOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyEqP(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine);

    extern void applyEqvP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine);

    extern void applyEqualP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine);
}

#endif
