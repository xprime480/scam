#if ! defined(APPLYARGSWORKER_HPP)
#define APPLYARGSWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;

    class ApplyArgsWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        ApplyArgsWorker(ScamExpr * op,
                        ScamExpr * args,
                        Continuation * cont,
                        Env * env);

        static ApplyArgsWorker * makeInstance(ScamExpr * op,
                                              ScamExpr * args,
                                              Continuation * cont,
                                              Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * op;
        ScamExpr * args;
        Continuation * cont;
        Env *        env;
    };

}

#endif
