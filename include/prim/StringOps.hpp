#if ! defined(STRINGOPS_HPP)
#define STRINGOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyString(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine);

    extern void applyMakeString(ScamValue args,
                                Continuation * cont,
                                ScamEngine * engine);

    extern void applyStringLength(ScamValue args,
                                  Continuation * cont,
                                  ScamEngine * engine);

    extern void applyStringRef(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine);

    extern void applyStringSetX(ScamValue args,
                                Continuation * cont,
                                ScamEngine * engine);

    extern void applyStringEqP(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine);

    extern void applyStringCiEqP(ScamValue args,
                                 Continuation * cont,
                                 ScamEngine * engine);

    extern void applyStringLtP(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine);

    extern void applyStringCiLtP(ScamValue args,
                                 Continuation * cont,
                                 ScamEngine * engine);

    extern void applyStringLeP(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine);

    extern void applyStringCiLeP(ScamValue args,
                                 Continuation * cont,
                                 ScamEngine * engine);

    extern void applyStringGtP(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine);

    extern void applyStringCiGtP(ScamValue args,
                                 Continuation * cont,
                                 ScamEngine * engine);

    extern void applyStringGeP(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine);

    extern void applyStringCiGeP(ScamValue args,
                                 Continuation * cont,
                                 ScamEngine * engine);

    extern void applyStringUpcase(ScamValue args,
                                  Continuation * cont,
                                  ScamEngine * engine);

    extern void applyStringDowncase(ScamValue args,
                                    Continuation * cont,
                                    ScamEngine * engine);
}

#endif
