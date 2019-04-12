#if ! defined(SCAMLETSTAR_H)
#define SCAMLETSTAR_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class ScamEngine;
    class MemoryManager;

    class LetStar : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;

        LetStar(ScamEngine * engine);
        static LetStar * makeInstance(ScamEngine * engine);

    public:
        static ScamExpr * safeCons(ScamExpr * expr);
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;

    private:
        ScamEngine * engine;
    };
}

#endif
