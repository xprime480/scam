#if ! defined(PRIMITIVE_CAR_HPP)
#define PRIMITIVE_CAR_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyCar(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine);
}

#endif
