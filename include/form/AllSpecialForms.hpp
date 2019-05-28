#if ! defined(ALLSPECIALFORMS_HPP)
#define ALLSPECIALFORMS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern const ScamValue spliceTag;

    extern void applyAmb(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine);

    extern void applyAnd(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine);

    extern void applyApply(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine);

    extern void applyAssign(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);

    extern void applyCallCC(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);

    extern void applyClassMaker(ScamValue args,
                                Continuation * cont,
                                Env * env,
                                ScamEngine * engine);

    extern void applyDefine(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);

    extern void applyEval(ScamValue args,
                          Continuation * cont,
                          Env * env,
                          ScamEngine * engine);

    extern void applyIf(ScamValue args,
                        Continuation * cont,
                        Env * env,
                        ScamEngine * engine);

    extern void applyLambda(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);

    extern void applyLet(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine);

    extern void applyLetRec(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);

    extern void applyLetStar(ScamValue args,
                             Continuation * cont,
                             Env * env,
                             ScamEngine * engine);

    extern void applyMacro(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine);

    extern void applyNot(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine);

    extern void applyOr(ScamValue args,
                        Continuation * cont,
                        Env * env,
                        ScamEngine * engine);

    extern void applyQuasiQuote(ScamValue args,
                                Continuation * cont,
                                Env * env,
                                ScamEngine * engine);

    extern void applyQuote(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine);

    extern void applyUndefine(ScamValue args,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine);

    extern ScamValue safeCons(ScamValue expr);
}

#endif
