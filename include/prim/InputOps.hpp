#if ! defined(INPUTOPS_HPP)
#define INPUTOPS_HPP 1

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
