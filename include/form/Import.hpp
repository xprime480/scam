#if ! defined(IMPORT_HPP)
#define IMPORT_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyImport(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);

    extern ScamValue importToEnv(ScamValue args, ScamEngine * engine);
}

#endif
