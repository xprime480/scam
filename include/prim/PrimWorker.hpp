#if ! defined(PRIMWORKER_HPP)
#define PRIMWORKER_HPP 1

#include "Worker.hpp"

#include "prim/PrimWorkerData.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;
    class Primitive;

    class  PrimWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        PrimWorker(Continuation * cont,
                   Env * env,
                   ScamExpr * args,
                   Primitive * caller);

        static PrimWorker * makeInstance(Continuation * cont,
                                         Env * env,
                                         ScamExpr * args,
                                         Primitive * caller);

    public:
        void mark() const override;
        void run() override;

    private:
        PrimWorkerData data;
    };
}

#endif
