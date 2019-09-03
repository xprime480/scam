#if ! defined(OUTPUTOPS_HPP)
#define OUTPUTOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyDisplay(ScamValue args, Continuation * cont);
    extern void applyNewline(ScamValue args, Continuation * cont);
}

#endif
