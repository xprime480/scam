#if ! defined(CALLCONT_HPP)
#define CALLCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class ScamEngine;

    class CallCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        CallCont(Continuation * cont, Env * env);
        static CallCont * makeInstance(Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        Continuation * cont;
        Env          * env;
    };
}

#endif
