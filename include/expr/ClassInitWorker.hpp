#if ! defined(CLASSINITWORKER_HPP)
#define CLASSINITWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ScamInstance;

    class ClassInitWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ClassInitWorker(ExprHandle instance,
                        ExprHandle args,
                        Continuation * cont,
                        Env * env);

        static ClassInitWorker * makeInstance(ExprHandle instance,
                                              ExprHandle args,
                                              Continuation * cont,
                                              Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle     instance;
        ExprHandle     args;
        Continuation * cont;
        Env          * env;
    };
}

#endif
