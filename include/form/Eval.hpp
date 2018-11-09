#if ! defined(SCAMEVAL_H)
#define SCAMEVAL_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Eval : public SpecialForm
    {
    public:
        Eval();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
