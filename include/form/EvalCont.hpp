#if ! defined(EVALCONT_HPP)
#define EVALCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class MemoryManager;

    class EvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        EvalCont(Continuation * cont, Env * env);
        static EvalCont * makeInstance(Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        Continuation * cont;
        Env * env;
    };
}

#endif
