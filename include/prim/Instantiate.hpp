#if ! defined(INSTANTIATE_HPP)
#define INSTANTIATE_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyInstantiate(ScamValue args,
                                 Continuation * cont,
                                 ScamEngine * engine);
}

#endif
