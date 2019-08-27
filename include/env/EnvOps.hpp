#if ! defined(ENVOPS_HPP)
#define ENVOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern Env * getConfigurationEnv(ScamEngine * engine);
    extern Env * getSyntaxEnv(ScamEngine * engine, Env * base);
    extern void initalizeLibraries(ScamEngine * engine, Env * base);
    extern Env * makeInteractionEnv(ScamEngine * engine, Env * base);

    extern void applyInteractionEnv(ScamValue args,
                                    Continuation * cont,
                                    ScamEngine * engine);

    extern void applyEval(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine);
}

#endif
