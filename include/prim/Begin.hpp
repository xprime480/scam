#if ! defined(PRIMITIVE_BEGIN_H)
#define PRIMITIVE_BEGIN_H 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyBegin(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine);
}

#endif
