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
        CarContinuation(WorkerData const & data, ScamEngine * engine);

        static CarContinuation *
        makeInstance(WorkerData const & data, ScamEngine * engine);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        WorkerData data;
    };
}

#endif
