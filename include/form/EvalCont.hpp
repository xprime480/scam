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

        EvalCont(Continuation * cont, Env * env, ScamEngine * engine);

        static EvalCont *
        makeInstance(Continuation * cont, Env * env, ScamEngine * engine);

    public:
        void mark() const override;
        void handleValue(ScamValue value) override;

    private:
        Continuation * cont;
        Env * env;
    };
}

#endif
