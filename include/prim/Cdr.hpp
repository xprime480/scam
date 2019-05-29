#if ! defined(PRIMITIVE_CDR_HPP)
#define PRIMITIVE_CDR_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyCdr(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine);
}

#endif
