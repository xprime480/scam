#if ! defined(NOTWORKER_HPP)
#define NOTWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;

    class NotWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        NotWorker(Continuation * cont, Env * env, ScamExpr * args);

        static NotWorker *
        makeInstance(Continuation * cont, Env * env, ScamExpr * args);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env * env;
    };
}

#endif
