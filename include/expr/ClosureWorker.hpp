#if ! defined(CLOSUREWORKER_HPP)
#define CLOSUREWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ClosureWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ClosureWorker(ScamValue closure,
                      Continuation * cont,
                      ScamValue args,
                      Env * argEnv,
                      ScamEngine * engine);

        static ClosureWorker * makeInstance(ScamValue closure,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv,
                                            ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      closure;
        Continuation * cont;
        ScamValue      args;
        Env          * argEnv;
    };
}

#endif
