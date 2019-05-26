#if ! defined(EVALOPS_HPP)
#define EVALOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    ScamValue withEnvUpdate(ScamValue value, Env * updated);
}

#endif
