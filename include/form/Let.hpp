#if ! defined(SCAMLET_H)
#define SCAMLET_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class ScamEngine;

    class Let : public SpecialForm
    {
    public:
        Let();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };

    class LetStar : public SpecialForm
    {
    public:
        LetStar(ScamEngine * engine);

        void apply(ScamExpr * args, ContHandle cont, Env env) override;

    private:
        ScamEngine * engine;
    };

    class LetRec : public SpecialForm
    {
    public:
        LetRec();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
