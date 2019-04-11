#if ! defined(SCAMLET_H)
#define SCAMLET_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class ScamEngine;

    class Let : public SpecialForm
    {
    private:
        Let();

    public:
        static Let * makeInstance();
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;
    };

    class LetStar : public SpecialForm
    {
    private:
        LetStar(ScamEngine * engine);

    public:
        static LetStar * makeInstance(ScamEngine * engine);
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;

    private:
        ScamEngine * engine;
    };

    class LetRec : public SpecialForm
    {
    private:
        LetRec();

    public:
        static LetRec * makeInstance();
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;
    };
}

#endif
