#if ! defined(EVALCONTINUATION_HPP)
#define EVALCONTINUATION_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;

    class PrimEvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        PrimEvalCont(ScamValue caller, Continuation * cont);

        static PrimEvalCont *
        makeInstance(ScamValue caller, Continuation * cont);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue      caller;
        Continuation * cont;
    };
}

#endif
