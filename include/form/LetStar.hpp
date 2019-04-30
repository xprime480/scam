#if ! defined(SCAMLETSTAR_H)
#define SCAMLETSTAR_H 1

#include "form/SpecialForm.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetStar : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;

        LetStar(ScamEngine * engine);
        static LetStar * makeInstance(ScamEngine * engine);

    public:
        static ExprHandle safeCons(ExprHandle expr);

        void apply(ExprHandle args, Continuation * cont, Env * env) override;

    private:
        ScamEngine * engine;
    };
}

#endif
