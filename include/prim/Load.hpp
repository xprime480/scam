#if ! defined(PRIMITIVE_LOAD_H)
#define PRIMITIVE_LOAD_H 1

#include "ScamFwd.hpp"

namespace scam
{
    extern const ScamValue fileErrorCategory;

    extern void applyLoad(ScamValue args, Continuation * cont);
}

#endif
