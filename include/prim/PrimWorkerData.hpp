#if ! defined(PRIMWORKERDATA_HPP)
#define PRIMWORKERDATA_HPP 1

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class Primitive;

    struct PrimWorkerData
    {
        PrimWorkerData(ScamExpr * args,
                       Continuation * original,
                       Env * env,
                       Primitive * caller);

        ScamExpr * args;
        Continuation * original;
        Continuation * cont;
        Env * env;
        Primitive * caller;

        void mark() const;

        void mapEval();
        void handleResult(ScamExpr * expr);
    };
}

#endif
