#if ! defined(LETEVALWORKER_HPP)
#define LETEVALWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;

    class LetEvalWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        LetEvalWorker(ScamExpr * formals,
                      ScamExpr * evaled,
                      ScamExpr * args,
                      ScamExpr * forms,
                      Continuation * cont,
                      Env * env,
                      bool rebind);

        static LetEvalWorker * makeInstance(ScamExpr * formals,
                                            ScamExpr * evaled,
                                            ScamExpr * args,
                                            ScamExpr * forms,
                                            Continuation * cont,
                                            Env * env,
                                            bool rebind);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * formals;
        ScamExpr * evaled;
        ScamExpr * args;
        ScamExpr * forms;
        Continuation * cont;
        Env * env;
        bool rebind;
    };
}

#endif
