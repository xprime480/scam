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

        ClassInitWorker(ScamValue instance,
                        ScamValue args,
                        Continuation * cont,
                        Env * env,
                        ScamEngine * engine);

        static ClassInitWorker * makeInstance(ScamValue instance,
                                              ScamValue args,
                                              Continuation * cont,
                                              Env * env,
                                              ScamEngine * engine);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamValue      instance;
        ScamValue      args;
        Continuation * cont;
        Env          * env;
    };
}

#endif
