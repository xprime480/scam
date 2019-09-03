#if ! defined(STRINGOPS_HPP)
#define STRINGOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyString(ScamValue args, Continuation * cont);
    extern void applyMakeString(ScamValue args, Continuation * cont);
    extern void applyStringLength(ScamValue args, Continuation * cont);
    extern void applyStringRef(ScamValue args, Continuation * cont);
    extern void applyStringSetX(ScamValue args, Continuation * cont);
    extern void applyStringEqP(ScamValue args, Continuation * cont);
    extern void applyStringCiEqP(ScamValue args, Continuation * cont);
    extern void applyStringLtP(ScamValue args, Continuation * cont);
    extern void applyStringCiLtP(ScamValue args, Continuation * cont);
    extern void applyStringLeP(ScamValue args, Continuation * cont);
    extern void applyStringCiLeP(ScamValue args, Continuation * cont);
    extern void applyStringGtP(ScamValue args, Continuation * cont);
    extern void applyStringCiGtP(ScamValue args, Continuation * cont);
    extern void applyStringGeP(ScamValue args, Continuation * cont);
    extern void applyStringCiGeP(ScamValue args, Continuation * cont);
    extern void applyStringUpcase(ScamValue args, Continuation * cont);
    extern void applyStringDowncase(ScamValue args, Continuation * cont);
    extern void applyStringCopy(ScamValue args, Continuation * cont);
    extern void applyStringAppend(ScamValue args, Continuation * cont);
    extern void applyString2List(ScamValue args, Continuation * cont);
    extern void applyList2String(ScamValue args, Continuation * cont);
    extern void applyStringCopyX(ScamValue args, Continuation * cont);
    extern void applyStringFillX(ScamValue args, Continuation * cont);
}

#endif
