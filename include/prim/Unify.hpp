#if ! defined(UNIFY_HPP)
#define UNIFY_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyUnify(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine);
}

#endif
