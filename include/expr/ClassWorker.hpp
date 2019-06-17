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

        ClassWorker(ScamValue cls,
                    ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine);

        static ClassWorker * makeInstance(ScamValue cls,
                                          ScamValue args,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      cls;
        ScamValue      args;
        Continuation * cont;
        Env          * env;
    };
}

#endif
