#if ! defined(EVALCONT_HPP)
#define EVALCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class EvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        EvalCont(Continuation * cont, Env * env);
        static EvalCont * makeInstance(Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        Continuation * cont;
        Env * env;
    };
}

#endif
