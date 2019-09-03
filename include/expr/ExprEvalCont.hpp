#if ! defined(EVALCONTINUATION_HPP)
#define EVALCONTINUATION_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;

    class ExprEvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ExprEvalCont(ScamValue cdr, Continuation * cont, Env * env);

        static ExprEvalCont *
        makeInstance(ScamValue cdr, Continuation * cont, Env * env);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue      cdr;
        Continuation * cont;
        Env          * env;
    };
}

#endif
