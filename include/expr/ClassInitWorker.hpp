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

        ClassInitWorker(ScamInstance * instance,
                        ExprHandle args,
                        Continuation * cont,
                        Env * env);

        static ClassInitWorker * makeInstance(ScamInstance * instance,
                                              ExprHandle args,
                                              Continuation * cont,
                                              Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamInstance * instance;
        ExprHandle     args;
        Continuation * cont;
        Env          * env;
    };
}

#endif
