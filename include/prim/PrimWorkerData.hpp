#if ! defined(PRIMWORKERDATA_HPP)
#define PRIMWORKERDATA_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    class Primitive;

    struct PrimWorkerData
    {
        PrimWorkerData(ExprHandle args,
                       Continuation * original,
                       Env * env,
                       Primitive * caller);

        ExprHandle args;
        Continuation * original;
        Continuation * cont;
        Env * env;
        Primitive * caller;

        void mark() const;

        void mapEval() const;
        void handleResult(ExprHandle expr);
    };
}

#endif
