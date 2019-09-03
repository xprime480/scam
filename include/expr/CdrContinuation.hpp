#if ! defined(CDRCONTINUATION_HPP)
#define CDRCONTINUATION_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class CdrContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        CdrContinuation(ScamValue car, Continuation * original);

        static CdrContinuation *
        makeInstance(ScamValue car, Continuation * original);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue      car;
        Continuation * original;
    };
}

#endif
