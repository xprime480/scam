#if ! defined(ANDWORKER_HPP)
#define ANDWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class AndWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        AndWorker(Continuation * cont, Env * env, ExprHandle args, size_t n);

        static AndWorker *
        makeInstance(Continuation * cont, Env * env, ExprHandle args, size_t n);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle args;
        Continuation * cont;
        Env * env;
        size_t n;
    };
}

#endif
