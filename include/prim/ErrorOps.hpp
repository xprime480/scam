#if ! defined(ERROROPS_HPP)
#define ERROROPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyMakeError(ScamValue args, Continuation * cont);
    extern void applyError(ScamValue args, Continuation * cont);
    extern void applyRaise(ScamValue args, Continuation * cont);
    extern void applyWithHandler(ScamValue args, Continuation * cont);
    extern void applyErrorMessage(ScamValue args, Continuation * cont);
    extern void applyErrorIrritant(ScamValue args, Continuation * cont);
    extern void applyReadErrorP(ScamValue args, Continuation * cont);
    extern void applyFileErrorP(ScamValue args, Continuation * cont);
    extern void applyErrorCat(ScamValue args, Continuation * cont);
    extern void applyError2String(ScamValue args, Continuation * cont);
}

#endif
