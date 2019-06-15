#if ! defined(PRIMWORKERDATA_HPP)
#define PRIMWORKERDATA_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    class Primitive;

    struct PrimWorkerData
    {
        PrimWorkerData(ScamValue args,
                       Continuation * original,
                       Env * env,
                       ScamValue caller);

        ScamValue args;
        Continuation * original;
        Continuation * cont;
        Env * env;
        ScamValue caller;

        void mark() const;

        void mapEval(ScamEngine * engine) const;
        void handleResult(ScamValue expr, ScamEngine * engine);
    };
}

#endif
