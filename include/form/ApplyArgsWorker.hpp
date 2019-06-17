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

        ApplyArgsWorker(ScamValue op,
                        ScamValue args,
                        Continuation * cont,
                        Env * env,
                        ScamEngine * engine);

        static ApplyArgsWorker * makeInstance(ScamValue op,
                                              ScamValue args,
                                              Continuation * cont,
                                              Env * env,
                                              ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue op;
        ScamValue args;
        Continuation * cont;
        Env *        env;
    };

}

#endif
