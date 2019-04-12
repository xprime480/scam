#if ! defined(ORWORKER_HPP)
#define ORWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;

    class OrWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        OrWorker(Continuation * cont, Env * env, ScamExpr * args, size_t n);

        static OrWorker *
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
