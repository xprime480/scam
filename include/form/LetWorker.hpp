#if ! defined(LETWORKER_HPP)
#define LETWORKER_HPP 1

#include "LetBaseWorker.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;

    class LetWorker : public LetBaseWorker
    {
    private:
        friend class scam::MemoryManager;
        LetWorker(ScamExpr * args, Continuation * cont, Env * env, bool rebind);

        static LetWorker * makeInstance(ScamExpr * args,
                                        Continuation * cont,
                                        Env * env,
                                        bool rebind);

    protected:
        void do_next(ScamExpr * formals,
                     ScamExpr * values,
                     ScamExpr * forms) override;

    private:
        const bool rebind;
    };
}

#endif
