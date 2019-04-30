#if ! defined(ORWORKER_HPP)
#define ORWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class OrWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        OrWorker(Continuation * cont, Env * env, ExprHandle args, size_t n);

        static OrWorker *
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
