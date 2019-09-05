#if ! defined(ALLSPECIALFORMS_HPP)
#define ALLSPECIALFORMS_HPP 1

#include "ScamFwd.hpp"

#include "form/Library.hpp"

namespace scam
{
    extern const char * spliceValue;

    extern void applyAmb(ScamValue args, Continuation * cont, Env * env);
    extern void applyAnd(ScamValue args, Continuation * cont, Env * env);
    extern void applyApply(ScamValue args, Continuation * cont, Env * env);
    extern void applyCallCC(ScamValue args, Continuation * cont, Env * env);
    extern void applyClassMaker(ScamValue args, Continuation * cont, Env * env);
    extern void applyDefine(ScamValue args, Continuation * cont, Env * env);

    extern void
    applyDefineSyntax(ScamValue args, Continuation * cont, Env * env);

    extern void applyIf(ScamValue args, Continuation * cont, Env * env);
    extern void applyLambda(ScamValue args, Continuation * cont, Env * env);
    extern void applyLet(ScamValue args, Continuation * cont, Env * env);
    extern void applyLetRec(ScamValue args, Continuation * cont, Env * env);
    extern void applyLetStar(ScamValue args, Continuation * cont, Env * env);
    extern void applyNot(ScamValue args, Continuation * cont, Env * env);
    extern void applyOr(ScamValue args, Continuation * cont, Env * env);
    extern void applyQuasiQuote(ScamValue args, Continuation * cont, Env * env);
    extern void applyQuote(ScamValue args, Continuation * cont, Env * env);
    extern void applySetX(ScamValue args, Continuation * cont, Env * env);

    extern void
    applySyntaxExpand(ScamValue args, Continuation * cont, Env * env);

    extern void applyUndefine(ScamValue args, Continuation * cont, Env * env);

    extern ScamValue safeCons(ScamValue expr);
}

#endif
