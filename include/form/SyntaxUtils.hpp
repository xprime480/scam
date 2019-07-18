#if ! defined(SYNTAXUTILS_HPP)
#define SYNTAXUTILS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern bool installSyntax(Env * env,
                              ScamEngine * engine,
                              ScamValue symbol,
                              ScamValue rules);

    extern void applySyntax(ScamValue value,
                            ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);
}

#endif
