#if ! defined(EVALCONTINUATION_HPP)
#define EVALCONTINUATION_HPP 1

#include "Continuation.hpp"

#include "prim/PrimWorkerData.hpp"

namespace scam
{
    class MemoryManager;

    class PrimEvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        PrimEvalCont(PrimWorkerData const & data);

        static PrimEvalCont * makeInstance(PrimWorkerData const & data);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        PrimWorkerData data;
    };
}

#endif
