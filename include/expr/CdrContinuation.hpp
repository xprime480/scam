#if ! defined(CDRCONTINUATION_HPP)
#define CDRCONTINUATION_HPP 1

#include "Continuation.hpp"

#include "expr/WorkerData.hpp"

namespace scam
{
    class Env;
    class ScamExpr;
    class MemoryManager;

    class CdrContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        CdrContinuation(WorkerData const & data);
        static CdrContinuation * makeInstance(WorkerData const & data);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        WorkerData data;
    };
}

#endif
