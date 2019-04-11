#if ! defined(EVALWORKER_H)
#define EVALWORKER_H 1

#include "Worker.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;

    class EvalWorker : public Worker
    {
    public:
        EvalWorker(ScamExpr * forms, Env * env, Continuation * cont);

        void run() override;

    private:
        ScamExpr * forms;
        Env * extended;
        Continuation * cont;
    };
}

#endif
