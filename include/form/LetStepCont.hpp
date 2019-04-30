#if ! defined(LETSTEPCONT_HPP)
#define LETSTEPCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetStepCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        LetStepCont(ExprHandle formals,
                    ExprHandle forms,
                    ExprHandle evaled,
                    ExprHandle args,
                    Continuation * cont,
                    Env * env,
                    bool rebind);

        static LetStepCont * makeInstance(ExprHandle formals,
                                          ExprHandle forms,
                                          ExprHandle evaled,
                                          ExprHandle args,
                                          Continuation * cont,
                                          Env * env,
                                          bool rebind);

    public:
        void mark() const override;

        void run(ExprHandle expr) override;

    private:
        ExprHandle formals;
        ExprHandle forms;
        ExprHandle evaled;
        ExprHandle args;
        Continuation * cont;
        Env * env;
        bool rebind;
    };
}

#endif
