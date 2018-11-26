#if ! defined(SCAMAMB_H)
#define SCAMAMB_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class ScamEngine;

    class Amb : public SpecialForm
    {
    public:
        Amb(ScamEngine * engine);

        void apply(ScamExpr * args, ContHandle cont, Env env) override;

    private:
        ScamEngine * engine;
    };
}

#endif
