#if ! defined(SCAMCONTINUATION_H)
#define SCAMCONTINUATION_H 1

#include "expr/ScamExpr.hpp"

#include "Continuation.hpp"

namespace scam
{
    class Continuation;

    class ScamContinuation : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamContinuation(ContHandle cont);
        ScamContinuation(Continuation * cont);
        static ScamContinuation * makeInstance(ContHandle cont);
        static ScamContinuation * makeInstance(Continuation * cont);

    public:
        void mark() const override;

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, ContHandle cont, Env env) override;

    private:
        Continuation * cont;
        ContHandle cOld;
    };
}

#endif
