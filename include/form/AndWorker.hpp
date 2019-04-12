#if ! defined(ANDWORKER_HPP)
#define ANDWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;

    class AndWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        AndWorker(Continuation * cont, Env * env, ScamExpr * args, size_t n);

        static AndWorker *
        makeInstance(Continuation * cont, Env * env, ScamExpr * args, size_t n);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env * env;
        size_t n;
    };
}

#endif
