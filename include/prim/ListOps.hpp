#if ! defined(PRIMITIVE_LIST_HPP)
#define PRIMITIVE_LIST_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyMakeList(ScamValue args, Continuation * cont);
    extern void applyList(ScamValue args, Continuation * cont);
    extern void applyAppend(ScamValue args, Continuation * cont);
}

#endif
