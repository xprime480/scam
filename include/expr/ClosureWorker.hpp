#if ! defined(CLOSUREWORKER_HPP)
#define CLOSUREWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LambdaParser;

    class ClosureWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ClosureWorker(LambdaParser * parser,
                      Env * capture,
                      Continuation * cont,
                      ScamValue args,
                      Env * argEnv,
                      bool macrolike,
                      ScamEngine * engine);

        static ClosureWorker * makeInstance(LambdaParser * parser,
                                            Env * capture,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv,
                                            bool macrolike,
                                            ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        LambdaParser * parser;
        Env          * capture;
        Continuation * cont;
        ScamValue      args;
        Env          * argEnv;
        bool           macrolike;
    };
}

#endif
