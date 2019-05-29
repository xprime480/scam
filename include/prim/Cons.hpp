#if ! defined(PRIMITIVE_CONS_HPP)
#define PRIMITIVE_CONS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyCons(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine);
}

#endif
