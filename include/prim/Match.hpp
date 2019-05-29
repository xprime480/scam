#if ! defined(MATCH_HPP)
#define MATCH_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyMatch(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine);
}

#endif
