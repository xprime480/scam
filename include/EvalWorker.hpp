#if ! defined(EVALWORKER_H)
#define EVALWORKER_H 1

#include "Worker.hpp"

#include "Env.hpp"

#include <memory>

namespace scam
{
    class ScamExpr;

    class Continuation;
    using ContHandle = std::shared_ptr<Continuation>;

    class EvalWorker : public Worker
    {
    public:
        EvalWorker(ScamExpr * forms, Env env, ContHandle cont);

        void run() override;

    private:
        ExprHandle forms;
        Env extended;
        ContHandle cont;
    };
}

#endif
