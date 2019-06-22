#if ! defined(SYNTAXUTILS_HPP)
#define SYNTAXUTILS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern bool installSyntax(Env * env,
                              ScamEngine * engine,
                              ScamValue symbol,
                              ScamValue rules);
}

#endif
