#if ! defined(EVALWORKER_H)
#define EVALWORKER_H 1

#include "Worker.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class MemoryManager;

    class EvalWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        EvalWorker(ScamExpr * forms, Env * env, Continuation * cont);

        static EvalWorker *
        makeInstance(ScamExpr * forms, Env * env, Continuation * cont);

    public:
       void mark() const override;
       void run() override;

    private:
        ScamExpr * forms;
        Env * extended;
        Continuation * cont;
    };
}

#endif
