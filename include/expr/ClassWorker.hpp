#if ! defined(CLASSWORKER_HPP)
#define CLASSWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ScamClass;

    class ClassWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ClassWorker(ExprHandle cls,
                    ExprHandle args,
                    Continuation * cont,
                    Env * env);

        static ClassWorker * makeInstance(ExprHandle cls,
                                          ExprHandle args,
                                          Continuation * cont,
                                          Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle     cls;
        ExprHandle     args;
        Continuation * cont;
        Env          * env;
    };
}

#endif
