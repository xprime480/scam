#if ! defined(LETEVALWORKER_HPP)
#define LETEVALWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetEvalWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        LetEvalWorker(ExprHandle formals,
                      ExprHandle evaled,
                      ExprHandle args,
                      ExprHandle forms,
                      Continuation * cont,
                      Env * env,
                      bool rebind);

        static LetEvalWorker * makeInstance(ExprHandle formals,
                                            ExprHandle evaled,
                                            ExprHandle args,
                                            ExprHandle forms,
                                            Continuation * cont,
                                            Env * env,
                                            bool rebind);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle formals;
        ExprHandle evaled;
        ExprHandle args;
        ExprHandle forms;
        Continuation * cont;
        Env * env;
        bool rebind;
    };
}

#endif
