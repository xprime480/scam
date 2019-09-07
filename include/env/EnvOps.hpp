#if ! defined(ENVOPS_HPP)
#define ENVOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern Env * getConfigurationEnv();
    extern Env * getSyntaxEnv(Env * base);
    extern void initalizeLibraries(Env * base);
    extern Env * makeInteractionEnv(Env * base);

    extern void applyInteractionEnv(ScamValue args, Continuation * cont);
    extern void applyEnvironment(ScamValue args, Continuation * cont);
    extern void applyEval(ScamValue args, Continuation * cont);
}

#endif
