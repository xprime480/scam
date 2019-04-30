#if ! defined(EVALCONTINUATION_HPP)
#define EVALCONTINUATION_HPP 1

#include "Continuation.hpp"

#include "expr/WorkerData.hpp"

namespace scam
{
    class MemoryManager;

    class ExprEvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        ExprEvalCont(WorkerData const & data);
        static ExprEvalCont * makeInstance(WorkerData const & data);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        WorkerData data;
    };
}

#endif
