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

        ClosureWorker(const LambdaParser * parser,
                      Env * capture,
                      Continuation * cont,
                      ScamValue args,
                      Env * argEnv,
                      bool macrolike,
                      ScamEngine * engine);

        static ClosureWorker * makeInstance(const LambdaParser * parser,
                                            Env * capture,
                                            Continuation * cont,
                                            ScamValue args,
                                            Env * argEnv,
                                            bool macrolike,
                                            ScamEngine * engine);

    public:
        void mark() const override;
        void run() override;

    private:
        const LambdaParser * parser;
        Env * capture;
        Continuation * cont;
        ScamValue args;
        Env * argEnv;
        bool macrolike;
    };
}

#endif
