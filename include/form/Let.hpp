#if ! defined(SCAMLET_H)
#define SCAMLET_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Let : public SpecialForm
    {
    public:
        Let();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };

    class LetStar : public SpecialForm
    {
    public:
        LetStar();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
