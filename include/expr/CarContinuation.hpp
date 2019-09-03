#if ! defined(CARCONTINUATION_HPP)
#define CARCONTINUATION_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class CarContinuation : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        CarContinuation(ScamValue cdr, Continuation * cont, Env * env);

        static CarContinuation *
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
