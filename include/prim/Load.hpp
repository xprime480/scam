#if ! defined(PRIMITIVE_LOAD_H)
#define PRIMITIVE_LOAD_H 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyInclude(ScamValue args, Continuation * cont);
    extern void applyLoad(ScamValue args, Continuation * cont);

    extern ScamValue loadHelper(const char * filename);
}

#endif
