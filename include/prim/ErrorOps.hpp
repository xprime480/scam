#if ! defined(ERROROPS_HPP)
#define ERROROPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyMakeError(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine);

    extern void applyError(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine);

    extern void applyRaise(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine);

    extern void applyWithHandler(ScamValue args,
                                 Continuation * cont,
                                 ScamEngine * engine);

    extern void applyErrorMessage(ScamValue args,
                                  Continuation * cont,
                                  ScamEngine * engine);

    extern void applyErrorIrritant(ScamValue args,
                                   Continuation * cont,
                                   ScamEngine * engine);
}

#endif
