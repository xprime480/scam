#if ! defined(EVALWORKER_H)
#define EVALWORKER_H 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class EvalWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        EvalWorker(ExprHandle forms, Env * env, Continuation * cont);

        static EvalWorker *
        makeInstance(ExprHandle forms, Env * env, Continuation * cont);

    public:
       void mark() const override;
       void run() override;

    private:
        ExprHandle forms;
        Env * extended;
        Continuation * cont;
    };
}

#endif
