#if ! defined(PRIMITIVE_LOAD_H)
#define PRIMITIVE_LOAD_H 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyLoad(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine);
}

#endif
