#if ! defined(PRIMITIVE_EQUALP_H)
#define PRIMITIVE_EQUALP_H 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyEqualP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine);
}

#endif
