#if ! defined(CLOSUREWORKER_HPP)
#define CLOSUREWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class Continuation;
    class Env;
    class ScamExpr;
    class MemoryManager;

    class ClosureWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ClosureWorker(ScamExpr *formals,
                      ScamExpr * forms,
                      Env * capture,
                      Continuation * cont,
                      ScamExpr * args,
                      Env * argEnv,
                      bool macrolike);

        static ClosureWorker * makeInstance(ScamExpr *formals,
                                            ScamExpr * forms,
                                            Env * capture,
                                            Continuation * cont,
                                            ScamExpr * args,
                                            Env * argEnv,
                                            bool macrolike);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * formals;
        ScamExpr * forms;
        Env * capture;
        Continuation * cont;
        ScamExpr * args;
        Env * argEnv;
        bool macrolike;
    };
}

#endif
