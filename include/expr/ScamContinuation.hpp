#if ! defined(SCAMCONTINUATION_H)
#define SCAMCONTINUATION_H 1

#include "expr/ScamExpr.hpp"

#include "Continuation.hpp"

namespace scam
{
    class Continuation;

    class ScamContinuation : public ScamExpr
    {
    public:
        ScamContinuation(ContHandle cont);

    public:
        static ScamContinuation * makeInstance(ContHandle cont);

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, ContHandle cont, Env env) override;

    private:
        ContHandle cont;
    };
}

#endif
