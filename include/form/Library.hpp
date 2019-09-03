#if ! defined(LIBRARY_HPP)
#define LIBRARY_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void
    applyDefineLibrary(ScamValue args, Continuation * cont, Env * env);

    extern void applyImport(ScamValue args, Continuation * cont, Env * env);

    extern ScamValue defineLibrary(ScamValue args);
    extern ScamValue importToEnv(ScamValue args);
}

#endif
