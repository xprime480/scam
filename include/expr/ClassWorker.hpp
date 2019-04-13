#if ! defined(CLASSWORKER_HPP)
#define CLASSWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class Continuation;
    class Env;
    class ScamExpr;
    class MemoryManager;

    class ClassWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ClassWorker(ScamExpr * cls,
                    ScamExpr * args,
                    Continuation * cont,
                    Env * env);

        static ClassWorker * makeInstance(ScamExpr * cls,
                                          ScamExpr * args,
                                          Continuation * cont,
                                          Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * cls;
        ScamExpr * args;
        Continuation * cont;
        Env *        env;
    };
}

#endif
