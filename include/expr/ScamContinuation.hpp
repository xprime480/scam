#if ! defined(SCAMCONTINUATION_H)
#define SCAMCONTINUATION_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class Continuation;

    class ScamContinuation : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamContinuation(Continuation * cont);
        static ScamContinuation * makeInstance(Continuation * cont);

    public:
        void mark() const override;

        std::string toString() const override;

        void apply(ExprHandle args, Continuation * cont, Env * env) override;

    private:
        Continuation * cont;
    };
}

#endif
