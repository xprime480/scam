#if ! defined(SCAMAND_H)
#define SCAMAND_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class And : public SpecialForm
    {
    public:
        And();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
