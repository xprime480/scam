#if ! defined(PRIMITIVE_LIST_HPP)
#define PRIMITIVE_LIST_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyList(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine);
}

#endif
