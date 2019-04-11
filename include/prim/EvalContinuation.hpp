#if ! defined(EVALCONTINUATION_HPP)
#define EVALCONTINUATION_HPP 1

#include "Continuation.hpp"

#include "prim/PrimWorkerData.hpp"

namespace scam
{
    class MemoryManager;

    class EvalContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        EvalContinuation(PrimWorkerData const & data);

        static EvalContinuation *
        makeInstance(PrimWorkerData const & data);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        PrimWorkerData data;
    };
}

#endif
