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
                      ExprHandle args,
                      Env * argEnv,
                      bool macrolike);

        static ClosureWorker * makeInstance(const LambdaParser * parser,
                                            Env * capture,
                                            Continuation * cont,
                                            ExprHandle args,
                                            Env * argEnv,
                                            bool macrolike);

    public:
        void mark() const override;
        void run() override;

    private:
        const LambdaParser * parser;
        Env * capture;
        Continuation * cont;
        ExprHandle args;
        Env * argEnv;
        bool macrolike;
    };
}

#endif
