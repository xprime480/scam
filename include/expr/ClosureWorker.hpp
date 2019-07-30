#if ! defined(CLOSUREWORKER_HPP)
#define CLOSUREWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LambdaDef;

    class ClosureWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ClosureWorker(LambdaDef & parser,
                      Env * capture,
                      Continuation * cont,
                      ScamValue args,
                      Env * argEnv,
                      ScamEngine * engine);

        static ClosureWorker * makeInstance(LambdaDef & parser,
                                            Env * capture,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv,
                                            ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        LambdaDef    & lambda;
        Env          * capture;
        Continuation * cont;
        ScamValue      args;
        Env          * argEnv;
    };
}

#endif
