#if ! defined(READOPS_HPP)
#define READOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyRead(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine);

    extern void applyEofObject(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine);
}

#endif
