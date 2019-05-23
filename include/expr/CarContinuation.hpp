#if ! defined(CARCONTINUATION_HPP)
#define CARCONTINUATION_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"
#include "expr/WorkerData.hpp"

namespace scam
{
    class CarContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        CarContinuation(WorkerData const & data);
        static CarContinuation * makeInstance(WorkerData const & data);

    public:
        void mark() const override;

        void run(ScamValue expr) override;

    private:
        WorkerData data;
    };
}

#endif
