#if ! defined(IFWORKER_HPP)
#define IFWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class Continuation;
    class Env;
    class ScamExpr;

    class IfWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        IfWorker(Continuation * cont, Env * env, ScamExpr * args);

        static IfWorker *
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
