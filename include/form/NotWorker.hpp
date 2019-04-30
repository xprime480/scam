#if ! defined(NOTWORKER_HPP)
#define NOTWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class NotWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        NotWorker(Continuation * cont, Env * env, ExprHandle args);

        static NotWorker *
        makeInstance(Continuation * cont, Env * env, ExprHandle args);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle args;
        Continuation * cont;
        Env * env;
    };
}

#endif
