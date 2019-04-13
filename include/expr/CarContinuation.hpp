#if ! defined(CARCONTINUATION_HPP)
#define CARCONTINUATION_HPP 1

#include "Continuation.hpp"
#include "expr/WorkerData.hpp"

namespace scam
{
    class Congtinuation;
    class Env;
    class ScamExpr;
    class MemoryManager;

    class CarContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        CarContinuation(WorkerData const & data);
        static CarContinuation * makeInstance(WorkerData const & data);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        WorkerData data;
    };
}

#endif
