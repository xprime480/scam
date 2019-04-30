#if ! defined(SCAMAMB_H)
#define SCAMAMB_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class ScamEngine;

    class Amb : public SpecialForm
    {
    private:
        Amb(ScamEngine * engine);

    public:
        /* this should probably be private */
        static Amb * makeInstance(ScamEngine * engine);

        void apply(ExprHandle args, Continuation * cont, Env * env) override;

    private:
        ScamEngine * engine;
    };
}

#endif
