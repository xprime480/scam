#if ! defined(SCAMCALLCC_H)
#define SCAMCALLCC_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class CallCC : public SpecialForm
    {
    public:
        CallCC();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
