#if ! defined(CDRCONTINUATION_HPP)
#define CDRCONTINUATION_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"
#include "expr/WorkerData.hpp"

namespace scam
{
    class CdrContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        CdrContinuation(WorkerData const & data);
        static CdrContinuation * makeInstance(WorkerData const & data);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        WorkerData data;
    };
}

#endif
