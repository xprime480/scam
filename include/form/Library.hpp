#if ! defined(LIBRARY_HPP)
#define LIBRARY_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyDefineLibrary(ScamValue args,
                                   Continuation * cont,
                                   Env * env,
                                   ScamEngine * engine);

    extern void applyImport(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);

    extern ScamValue defineLibrary(ScamValue args, ScamEngine * engine);
    
    extern ScamValue importToEnv(ScamValue args, ScamEngine * engine);
}

#endif
