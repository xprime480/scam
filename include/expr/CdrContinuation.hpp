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

        CdrContinuation(WorkerData const & data, ScamEngine * engine);

        static CdrContinuation *
        makeInstance(WorkerData const & data, ScamEngine * engine);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        WorkerData data;
    };
}

#endif
