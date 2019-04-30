#if ! defined(APPLYARGSWORKER_HPP)
#define APPLYARGSWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ApplyArgsWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        ApplyArgsWorker(ExprHandle op,
                        ExprHandle args,
                        Continuation * cont,
                        Env * env);

        static ApplyArgsWorker * makeInstance(ExprHandle op,
                                              ExprHandle args,
                                              Continuation * cont,
                                              Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle op;
        ExprHandle args;
        Continuation * cont;
        Env *        env;
    };

}

#endif
