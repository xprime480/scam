#if ! defined(ENVOPS_HPP)
#define ENVOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern Env * getConfigurationEnv(ScamEngine * engine);
    extern Env * getInteractionEnv(ScamEngine * engine, Env * base);
}

#endif
