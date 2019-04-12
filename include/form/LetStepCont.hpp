#if ! defined(LETSTEPCONT_HPP)
#define LETSTEPCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    class Env;
    class ScamExpr;

    class LetStepCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        LetStepCont(ScamExpr * formals,
                    ScamExpr * forms,
                    ScamExpr * evaled,
                    ScamExpr * args,
                    Continuation * cont,
                    Env * env,
                    bool rebind);

        static LetStepCont * makeInstance(ScamExpr * formals,
                                          ScamExpr * forms,
                                          ScamExpr * evaled,
                                          ScamExpr * args,
                                          Continuation * cont,
                                          Env * env,
                                          bool rebind);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        ScamExpr * formals;
        ScamExpr * forms;
        ScamExpr * evaled;
        ScamExpr * args;
        Continuation * cont;
        Env * env;
        bool rebind;
    };
}

#endif
