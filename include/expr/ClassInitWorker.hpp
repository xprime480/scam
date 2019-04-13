#if ! defined(CLASSINITWORKER_HPP)
#define CLASSINITWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class Continuation;
    class Env;
    class ScamExpr;
    class MemoryManager;

    class ClassInitWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ClassInitWorker(ScamExpr * instance,
                        ScamExpr * args,
                        Continuation * cont,
                        Env * env);

        static ClassInitWorker * makeInstance(ScamExpr * instance,
                                              ScamExpr * args,
                                              Continuation * cont,
                                              Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * instance;
        ScamExpr * args;
        Continuation * cont;
        Env *        env;
    };
}

#endif
